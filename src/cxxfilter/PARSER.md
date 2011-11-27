Parsing the State Machine
=========================

Now that we have a valid state machine that describes the problem, we want to
transform this state machine into actual code.  Instead of attempting to write
an equivalent state machine in code, which could either introduce errors, or if
we find errors in our assumptions above, cause our documentation to become
outdated, we will write a very simple source-source compiler.  It sounds more
difficult than it actually is.  The advantage is that we won't have to build
state transition tables by hand, which as we will see in a bit, can be tedious.
Also, this will give us the flexibility of porting this parser easily to other
languages, and furthermore, of separating the concrete logic above from
implementation details below.

First, we will build a very simple tokenizer using [flex][flex_website] that
will transform the source file into a list of tokens.

<<xparse.l>>=

    %{
    #include <stdio.h>
    #include "y.tab.h"
    %}
    %%
    [\[][^\]]+[\]]        <<return MATCH>>
    state                   return STATE 
    transition              return TRANSITION
    using                   return USING
    class                   return CLASS
    ->                      return ARROW
    :default:               return DEFAULT_CHAR_CLASS
    {                       return OPEN_CURLY_BRACE
    }                       return CLOSE_CURLY_BRACE
    [A-Za-z][a-zA-Z0-9]+  <<return IDENTIFIER>>
    ::                      return PACKAGE_SEPARATOR_OPT
@

Our DSL has eleven different types of tokens.  From these tokens, we can
describe a parser in Yacc.

<<xparse.y>>=

    <<yacc header>>
    <<tokens>>
    <<parse structure>>
    <<types>>
    %%

    stateMachine: /* empty */
              | stateMachine stateDecl
                <<add state declaration>>
              | stateMachine transitionDecl
                <<add transition declaration>>
              ;

    <<state rules>>
    <<transition rules>>
@

The first rule in our grammar is the root stateMachine rule.  This is a simple
recursive declaration that can be empty, or can have a stateDecl or
transitionDecl.  By describing the rule in this way, we can string together any
number of state or transition declarations into a single state machine.  Note
that we are using left recursion here, which is much more efficient than right
recursion.  This is why we don't write "stateDecl stateMachine".  For more
information on why this is the case in yacc, please see
[this article][yacc_left_recursion].

As is the case in literate programming, we elided the next two declaration
categories (state rules and transition rules), which are described below.  The
bit at the top, the yacc header, is provided in the appendix.  It just provides
some simple boilerplate which is unnecessary to describe here.  The tokens are
used for applying certain semantic rules to tokens described in the lex grammar
so that we can parse them properly.  The final piece, the parse structure, will
be described in detail when we go over the actions below.

<<state rules>>=

    stateDecl:
            STATE IDENTIFIER OPEN_CURLY_BRACE stateStatements CLOSE_CURLY_BRACE
                <<build state>>
            ;

    stateStatements: /* empty */
              <<create empty state struct>>
            | stateStatements matchClause
              <<create match clause>>
            ;

    matchClause:
            matchStmt OPEN_CURLY_BRACE stateTransition CLOSE_CURLY_BRACE
                <<build match>>
            ;

    matchStmt: MATCH
                <<handle match chars>>
            |  DEFAULT_CHAR_CLASS
                <<handle default char class>>
            ;

    stateTransition:
            TRANSITION ARROW IDENTIFIER
                <<build transition statement>>
            ;
@

A state declaration is made up of a state block, and a set of state statements.
A state statement is made of a match clause, which is a set of characters that
cause a certain transition to occur, and a stateTransition, which is a specific
mechanism by which the state machine switches from one state to another, or from
the current state back to itself.

Next, we will describe the transition rules.  Transitions are where all of the
actual code side-effects occur in our parser.  These include writing input to
output streams, buffering potential luhn sequences for evaluation, performing
the luhn check, and scrubbing luhn digits.

<<transition rules>>=

    transitionDecl:
            TRANSITION IDENTIFIER OPEN_CURLY_BRACE transitionStatements CLOSE_CURLY_BRACE
                <<build transition>>
            ;

    transitionStatements: /* empty */
                <<create empty transition struct>>
            | transitionStatements usingClassDecl
                <<create using class statement>>
            | transitionStatements stateAssignment
                <<create state assignment statement>>
            ;

    usingClassDecl:
            USING CLASS className
                <<set using class>>
            ;

    className: IDENTIFIER
                <<set class name>>
            | className PACKAGE_SEPARATOR_OPT IDENTIFIER
                <<set qualified class name>>
            ;

    stateAssignment:
            STATE ARROW IDENTIFIER
                <<set state>>
            ;
@

As we can see from this definition, we are supporting two basic commands in
state transitions: a declaration naming the class that we will use for the
transition, and a state assignment operation for completing the state
transition.

Now that we have the basic grammar for our parser defined, we need to describe
how this grammar will be parsed.  Yacc utilizes callback actions to parse a
grammar.  In these actions, we must save information in a structure for use
later on when we have enough contextual information to emit code.  For those
familiar with parsing XML, Yacc is very similar to a SAX parser, where many
other parsers use an AST, which is reminiscent of a DOM parser.

Emitting C++ code
=================

From here down, we get into a specific transformation of the above grammar into
a source language.  This step could be moved into a separate document, and
additional implementations in different languages (e.g. Java, C#, Java byte
code, etc.) could become sibling documents.

We are going to build a very simple source-to-source compiler using Yacc, Lex,
and some C code.  While I do maintain my own compiler framework, it is not yet
documented enough for someone following this code to stop and search a reference
guide for a more in-depth treatment of what I am doing here.  I felt that
sticking to something well-documented at this point would help to ensure that
future maintainers have a place to start.  This is old technology, and many
newer systems could have been used in place of this.  However, a compiler
front-end can be a complicated enough bit of code that dealing with bells and
whistles of a modern compiler framework could detract from understanding the
basics of parsing a grammar and emitting code.  I chose C because it is the
traditional language for Yacc / Lex.  If others choose to extend this grammar,
then they will likely look towards the copious tutorials and examples that
already exist for these systems, and most will be in C.

At a later date, when my own compiler framework is ready for prime time, I may
revisit this code to show how much simpler this is to do with an AST-AST
transformer.  For now, however, Yacc is the more appropriate option.

Parser Actions
--------------

In order to generate code, we must first build up parser actions that will
reduce the leaves and branches of the source code into a parse tree.  This
requires a flexible data structure which we will call a parse structure.
Since the parse structure is re-used for every callback, it is mapped as a C
union.  The parser callbacks are responsible for understanding how to interpret
this union, which is actually much easier in practice than it might seem.  In
our union, we must track transitions, their classes, states, and which
characters cause which transitions to occur.  From this, we can emit an entire
parser from the root element in the grammar.

<<parse structure>>=

    %union {
        char* name;
        unsigned char* characters;
        XTransition *xTransition;
        XState *xState;
        XMatchChars *xMatchChar;
    }
@

Now, we need to spend some time discussing tokens.  In the lex parser, we
defined some tokens that we want to re-use in our yacc grammar.  As we look to
apply actions to this grammar, it makes sense to define our tokens alongside
these actions so that we can interpret them as data structures.

<<tokens>>=

    %token STATE TRANSITION USING CLASS ARROW
    %token OPEN_CURLY_BRACE CLOSE_CURLY_BRACE
    %token PACKAGE_SEPARATOR_OPT
    <<%token MATCH>>
    %token DEFAULT_CHAR_CLASS
    <<%token IDENTIFIER>>
@

Most of the tokens will just be interpreted as tokens in our grammar.  The two
tokens that we will expand upon are the MATCH, and IDENTIFIER tokens.  These
will require special interpretation, and we will consider each of these as we
come across actions that use them.

From here, it's best to describe the parse logic from the bottom up.  We will
start by describing the parse of transitions from setting the state up to adding
these transitions to the parse tree.  Then, we will do the same for state
descriptions.

The set state callback here occurs when a state assignment element is
encountered.  Here, we just save the state name for the next callback.

<<set state>>=

    {
        $$ = identifier($3);
    }
@

Here, $3 represents the IDENTIFIER portion of our grammar for stateAssignment.
For this to make sense to Yacc, we need to describe what this IDENTIFIER is.  In
our case, it's the name portion of the union.  So, we will define the IDENTIFIER
token as such.

<<%token IDENTIFIER>>=

    %token <name> IDENTIFIER
@

Now, for Lex to interpret this appropriately, we need to copy the data from the
scanner to our name, which is done by extending the lexer's return token as
such:

<<return IDENTIFIER>>=

    { yyval.name = strdup(*yytext); return IDENTIFIER; }
@

Here, we copy the text value into our name, and return the appropriate token to
the parser.  Moving up this chain, the next two operations are similar, but with
a slight twist.  In languages such as C++ and Java, we can have namespace or
package names that require additional special handling.  Since we want this
grammar to be able to handle such languages when referencing classes, it makes
sense to expose these package names in code.  When interpreting the className
element, this means that we have some work to do to reduce an arbitrarily
complex class / package name description into a single qualified name.  In C++,
we will use the double-colon to qualify the name.  In Java, we would replace
this delimiter with a dot.

First, let's consider the base case, a simple identifier:

<<set class name>>=

    { $$ = createSetState($1); }
@

This much is simple.  Find an identifier, and reduce it.  In fact, the action is
practically the same as for the set state action.  Now, let's handle class names
of arbitrary complexity.

<<set qualified class name>>=

    { $$ = createQualifiedClassName($1, $3); }
@

Here, we just append the two identifiers into a single identifier.  Piece of
cake.  For the sake of completion, we will just use the set using class action
to pass the qualified class name along for the ride.  We did all of the heavy
lifting in the last two actions.

<<set using class>>=

    { $$ = $3; }
@

Now, we need to build these two different types of transition statements into a
state transition block.  To do this, we need to reduce these two different types
of transitions into an XTransition structure.  We will do this by merging our
changes into a transition structure as the next two action steps.  As before,
we'll save the details for how these functions work until later.

<<create state assignment statement>>=

    { $$ = createStateAssignment($1, $2); }
@

Here, we just fold the state assignment into an already existing transition
block.

<<create using class statement>>=

    { $$ = createUsingClass($1, $2); }
@

Finally, we will handle the creation of the initial empty transition structure.

<<create empty transition struct>>=

    { $$ = createTransitionStruct(); }
@

With all of our statements in place, we can now pass our transition structure
along for the ride to the root element with a simple build transition action.
Here, we will also pick up the transition's name.

<<build transition>>=

    { $$ = createTransition($2, $4); }
@

This concludes the parsing of state transitions.  Now, we can consider how to
parse state blocks.  Again, we will start from the bottom up, by first building
a transition statement.

<<build transition statement>>=

    { $$ = createTransitionStatement($3) }
@

Next, we must consider how to handle match statements.  A match statement is a
grouping of characters that should cause a certain block to execute.  In our
case, we will treat these as a small subset of regular expression character
classes (square bracket character listings) and the :default: character class.

First, for the default character class:

<<handle default char class>>=

    { $$ = createDefaultMatchClause(); }
@

To handle the regular match char class, we must first define how to interpret
the match token in our lexical scanner.  In this case, we will interpret it as
a string, just like the identifier.  We will re-use the name variable here.

<<return MATCH>>=

    { yyval.name = strdup(*yytext); return MATCH; }
@

Additionally, we will let Yacc know that a MATCH token is stored in name, so
that it can handle our reduction below appropriately.

<<%token MATCH>>=

    %token <name> MATCH
@

Now, the reduction becomes a very simple statement in Yacc.

<<handle match chars>>=

    { $$ = createMatchChars($1) }
@

With a match statement and a state transition, we can now consider the whole
match clause.

<<build match>>=

    { $$ = createMatch($1, $3); }
@

Now, we need to fold all of our match clauses into an XState structure, which
will be used to create a complete state block in the application.  To do this,
we will merge each match clause into a single XState structure, using left
recursion as before.  First, the folding step:

<<create match clause>>=

    { $$ = foldMatchClauseIntoStateStatements($1, $2); }
@

Finally, to resolve our left recursion, we will create an empty state statement.

<<create empty state struct>>=

    { $$ = createEmptyStateStatements(); }
@

The state declaration will allow us to pick up the name of the state and fold
this into our state structure.

<<build state>>=

    { $$ = updateStateStatementsWithName($2, $4); }
@

Now, we have parsed both the state blocks and the transition blocks.  All that
is left is to combine both of these into a parse tree.  First, we'll add state
blocks to this tree.

<<add state declaration>>=

    { addStateDeclarationToParseTree($2); }
@

Finally, we'll add state transitions to our parse tree.

<<add transition declaration>>=

    { addTransitionDeclarationToParseTree($2); }
@

Before wrapping up this section, we must consider some typing information.  Yacc
needs to know the types of not only tokens, but of each element in the
grammar.  Since it is a rather simple front-end for a C compiler, it does not
have the ability to perform type inference, so we need to help it out a little.

So, we will expand on the types declared in the xparse.y block to provide type
information for each element in our grammar.

<<types>>=

    %type <xTransition> transitionDecl transitionStatements
    %type <name> usingClassDecl stateAssignment
    %type <name> className
    %type <xMatchChar> matchClause
    %type <xState> stateStatements stateDecl
    %type <name> matchStmt
    %type <name> stateTransition
@

At this point, parsing is complete.  Now, we can focus on building a syntax tree.

Building a Syntax Tree
----------------------

The output from a successful parse is an XParseTree structure, which will will
traverse in order to generate C++ source code.  Before we begin this process, we
should discuss some potential options for the generated code.

By far, one of the most efficient mechanisms that we can use to generate a state
machine is a jump table.  We store a pointer to this jump table as the current
state, and for each transition, we map a value (in this case, a byte) to a jump
location.  This jump location would update the application state, execute some
code, and perform the next transition.  The only problem with this model is that
there is no external way to control the transitions of state.

We are building a filter that takes input from a stream, filters it, and write
output to a different stream.  The easiest way to model such a filter is as a
function that takes an input byte and an output stream, and performs some
action.  We will also include an application state object for thread safety.

    EStatus filterByte(unsigned char byte, ApplicationState& state, std::ostream& out);

We'll come back to the problem of efficiency later.  Early optimization is evil
anyway.  Instead, let's consider how this design decision affects our state
machine implementation.  Since our efficient jump table machine does not easily
work within the boundaries of a function, it's better to build a function call
table.  Yet, our functions might want to save state.  For instance, we will need
to remember the previous digits stored as part of a Luhn sequence.  Therefore,
instead of a function call table, we will create a class table.  The classes
used in this table will be our state transition classes themselves.  Now, we can
efficiently switch states based on input bytes, and save state.  It's not as
fast as a jump table, but in practice, a virtual function call is pretty darn
fast.  Most importantly, because we are modeling a function call, we can place
certain constraints on how this call works in order to return control to the
original caller after each byte is filtered.  Additionally, we can return a
special status code if a Luhn sequence is discovered, allowing the caller to
take appropriate action, such as paging someone.

For simplicity sake, we'll let our compiler deal with the problem of generating
classes.  The output will be one header containing all of the defined classes
and the filter function, and a C++ source file, defining all of the
implementation details.  This will leave us to the task of writing some simple
transition callbacks for handling some details better left to programmers
instead of machines.

First, we'll define the header which contains our parse functions.

<<xparse.h>>=

    #ifndef  XPARSE_H_HEADER_GUARD
    #define  XPARSE_H_HEADER_GUARD

        <<struct XTransition>>
        <<struct XMatchChars>>
        <<struct XState>>

        void parseTree();
        void addStateDeclarationToParseTree(XState* state);
        void addTransitionDeclarationToParseTree(XTransition* transition);

        XState* updateStateStatementsWithName(char* name, XState* state);
        XState* createEmptyStateStatements();
        XState* foldMatchClauseIntoStateStatements(XState* state, XMatchChars* matchClause);
        XMatchChars* createMatch(char* matchStmt, char* stateTransition);
        char* createTransitionStatement(char* transitionName);
        char* createMatchChars(char* match_chars);
        char* createDefaultMatchClass();

        XTransition* createTransition(char* name, XTransition* transitionStatements);
        XTransition* createTransitionStruct();
        XTransition* createUsingClass(XTransition* transition, char* usingClassDecl);
        XTransition* createStateAssignment(XTransition* transition, char* stateAssignment);
        char* createQualifiedClassName(char* className, char* identifier);
        char* createSetState(char* state);

    #endif //XPARSE_H_HEADER_GUARD
@

The C file, xparse.c, will look practically identical to this file, except that
the function definitions will be elided.  It is available in the appendix.  We
will expand on each function to complete the building of the parse tree, and
then use this parse tree to generate C++ source code.

Again, we will work from the bottom up.  The createSetState function simply
passes along the state variable created by the lexer.  It has already been
copied to the heap, so it can just go along for the ride.

<<char* createSetState(char* state);>>=

    char* createSetState(char* state)
    {
        return state;
    }
@

The createQualifiedClassName function appends two strings together and returns
the result.  Since the memory for the two strings will no longer be used, it is
reclaimed.  The new string is sized to fit the two original strings plus the
separator string. 

<<char* createQualifiedClassName(char* className, char* identifier);>>=

    char* createQualifiedClassName(char* className, char* identifier)
    {
        size_t classNameSize = strlen(className);
        size_t identifierSize = strlen(identifier);
        size_t stringSize = strlen(className) + strlen(identifier) + 2;
        char* combinedString = (char*)malloc(stringSize);

        strncpy(combinedString, className, classNameSize);
        strncat(combinedString, ".", 1);
        strncat(combinedString, identifier, identifierSize);

        free(className);
        free(identifier);

        return combinedString;
    }
@

The action for state assignment simply reduced the result of the set state to
the state assignment element.  So, we don't have to write any additional code to
handle this.  Now, we can add this state assignment to our transition structure.

<<XTransition* createStateAssignment(XTransition* transition, char* stateAssignment);>>=

    XTransition* createStateAssignment(XTransition* transition, char* stateAssignment)
    {
        assert(transition != NULL);
        assignment->state = stateAssignment;

        return transition;
    }
@

We add an assert here to check that transition is not null, since it should have
been created for us already.

The createUsingClass function looks about the same, only it is handling our
using class case.

<<XTransition* createUsingClass(XTransition* transition, char* usingClassDecl);>>=

    XTransition* createUsingClass(XTransition* transition, char* usingClassDecl)
    {
        assert(transition != NULL);
        assignment->usingClass = usingClassDecl;

        return transition;
    }
@

Next, we have a function to create the initial transition structure.

<<XTransition* createTransitionStruct();>>=

    XTransition* createTransitionStruct()
    {
        XTransition* transition = (XTransition*)malloc(sizeof(XTransition));
        memset(transition, 0, sizeof(XTransition));

        return transition;
    }
@

Finally, we will give our transition block a name via the createTransition
function.  After this point, the transition block is complete and can be turned
into C++ code.

<<XTransition* createTransition(char* name, XTransition* transitionStatements);>>=

    XTransition* createTransition(char* name, XTransition* transitionStatements)
    {
        assert(transition != null);
        transition->name = name;

        return transition;
    }
@

We haven't yet defined the XTransition structure, but now that we have all of
the functions that will be operating on it defined, it is a simple formality of
gathering all of the fields into one place.  Additionally, we will define a
cleanup function for ensuring that this structure is properly disposed of.

<<struct XTransition>>=

    typedef struct struct_XTransition
    {
        char* name;
        char* usingClass;
        char* state;
    } XTransition;

    void freeXTransition(XTransition* transition);
@

This function will simply free each member of the XTransition structure, and
finally, free itself.  In C++, of course, we could take advantage of
destructors, but _c'est la vie_.

<<void freeXTransition(XTransition* transition);>>=

    void freeXTransition(XTransition* transition)
    {
        free(name);
        free(usingClass);
        free(state);

        free(transition);
    }
@

With transitions out of the way, we can now focus on the state blocks.  The
complicated part of dealing with state blocks is understanding that we have to
match multiple bytes to different transitions.  The generated code will be a
simple table lookup, but in order to properly generate this table lookup, we
must keep track of each set of characters so the generator can look for overlaps
and check to make sure that each byte is accounted for.  Therefore, we will
store each of these matching clauses as an entry in an array.  For simplicity
sake, we will make this a fixed size array in the XState structure, assuming a
worst case of 256 entries, which is quite excessive.

First, let's define the XMatchChars structure.

<<struct XMatchChars>>=

    typedef struct struct_XMatchChars
    {
        <<enum type;>>
        char* transitionName;
        unsigned char* characters;
        size_t charactersSize;
    } XMatchChars;

    void freeXMatchChar(XMatchChars* match);
@

Here, we take a dynamically allocated array of characters, along with a size.
Since this array could conceivably contain null, we don't want to map this as a
null terminated string.  Finally, we include a transition name, which will be
used to build the transition table.  The enumeration is used to determine
whether this is a match chars statement, or a default character class
statement.  It is defined as thus:

<<enum type;>>=

    enum XMatchCharType
    {
        E_DEFAULT,
        E_MATCH_CHARS
    } type;
@

No structure like this is complete without a mechanism to free it.  Here it is.

<<void freeXMatchChar(XMatchChars* match);>>=

    void freeXMatchChar(XMatchChars* match)
    {
        free(match->transitionName);
        free(match->characters);
        free(match);
    }
@

Now, let's consider the functions for generating matches.  First, we can handle
the simple case of dealing with a default match class statement.

<<char* createDefaultMatchClass();>>=

    char* createDefaultMatchClass()
    {
        return strdup(":default:");
    }
@

Duplicating the string, :default:, is a bit klunky, but it really just pushes
the logic required for handling character classes into a single place upstream,
which is nice if we decide to handle multiple character classes.  Match chars
will also forward a string along for now.

<<char* createMatchChars(char* match_chars);>>=

    char* createMatchChars(char* match_chars)
    {
        return match_chars;
    }
@

Next, we need to handle transition statements in the match clause.  Again, we
will just pass a character string upstream.

<<char* createTransitionStatement(char* transitionName);>>=

    char* createTransitionStatement(char* transitionName)
    {
        return transitionName;
    }
@

From the match statement and the transition statement, we can create a match
clause.  This is an XMatchChars structure.  We set the transition name, and
depending upon the match statement, which can either be the default character
class or a sequence of bytes, we will either make this a default character
class or transform the byte sequence into a buffer of bytes.  The latter
operation has been elided so we can treat it in depth below.

<<XMatchChars* createMatch(char* matchStmt, char* stateTransition);>>=

    <<unsigned char* transformMatchStatement(char* matchStmt, size_t* size);>>

    XMatchChars* createMatch(char* matchStmt, char* stateTransition)
    {
        XMatchChars* match = (XMatchChars*)malloc(sizeof(XMatchChars));
        memset(match, 0, sizeof(XMatchChars));

        if (!strcmp(matchStmt, ":default:"))
        {
            match->type = E_DEFAULT;
        }
        else
        {
            match->type = E_MATCH_CHARS;
            match->characters =
                transformMatchStatement(matchStmt, &match->characterSize);
        }

        match->transitionName = stateTransition;
        free(matchStmt); //reclaim memory

        return match;
    }
@

To transform a list of match characters to a match statement, we must handle
four cases: the beginning and end of the array, individual characters,
character ranges, and escaped characters.  Since there is a maximum of 256
bytes that we care about, we will pre-allocate a large enough buffer to handle
all bytes, then append to this buffer as we parse the string.  Note that our
job here now isn't to make a perfect parser, but rather one that can handle our
input sequence.  If things seem hairy in this function, keep in mind that it
was written to handle a very narrow subset of possible functionality, with
plenty of asserts when things seem wrong.

<<unsigned char* transformMatchStatement(char* matchStmt, size_t* size);>>=

    unsigned char* transformMatchStatement(char* matchStmt, size_t* size)
    {
        unsigned char* matchCharacters = (unsigned char*)malloc(256);
        *size = 0;

        while (*matchStmt)
        {
            switch (*matchStmt)
            {
                <<skip begin/end sequence>>
                <<handle escaped characters>>
                <<handle character range>>
                <<handle base case>>
            }
        }

        return matchCharacters;
    }
@

First, we can skip the begin and end sequence.  We'll just increment our pointer
and ignore these characters all together.

<<skip begin/end sequence>>=

    case '[':
    case ']':
        ++matchStmt;
        break;
@

Next, we will handle a small range of escape characters.  Note that we are not
supporting C-style escapes (e.g. \n \r \t).  These can be added later on, but
for now, they are unnecessary for parsing our state machine.  We also add an
assert check in here for the end of string, since a string like "\" would be
invalid.  If needed, hex or octal escape sequences could also be easily
supported here.

<<handle escaped characters>>=
    case '\\':
        //increment past the escape character
        ++matchStmt;
        switch (*matchStmt)
        {
            case '0':
                matchCharacters[*size] = 0;
                break;

            case 0:
                assert(!"Invalid escaped character.");
                break;

            default:
                matchCharacters[*size] = *matchStmt;
                break;
        }
        //increment to the next character in the sequence
        ++matchStmt;
        ++(*size);
        break;
@

The character range is another complicated case to handle.  We will assert over
some of the more exotic cases (a range of two escaped characters), and deal with
just the base case that we need for our state machine.

<<handle character range>>=

    case '-':
        ++matchStmt;

        switch (*matchStmt)
        {
            case 0:
            case ']':
            case '[':
            case '-':
            case '\\':
                assert(!"Invalid character range");
                break;

            default:
                //expand sequence
                assert(*size > 0);
                for (unsigned char i = matchCharacters[*size - 1] + 1;
                     i <= *matchStmt;
                     ++i)
                {
                    matchCharacters[*size] = i;
                    ++(*size);
                }
                //skip the end sequence character
                ++matchStmt;
        }
        break;
@

Finally, we can deal with the base case, which is a single character to be
appended to our array.

<<handle base case>>=

    default:
        matchCharacters[*size] = 0;
        ++(*size);
        ++matchStmt;
        break;
@

With our XMatchChars structures built, we can now fold these into an XState
structure.  Here, we begin building the parse tree for a state block.

<<XState* foldMatchClauseIntoStateStatements(XState* state, XMatchChars* matchClause);>>=

    XState* foldMatchClauseIntoStateStatements(XState* state, XMatchChars* matchClause)
    {
        assert(state->numMatchChars < MAX_MATCH_CHARS);
        state->matchChars[state->numMatchChars] = matchClause;
        ++state->numMatchChars;

        return state;
    }
@

Our initial XState array must be built as well as part of left recursion.

<<XState* createEmptyStateStatements();>>=

    XState* createEmptyStateStatements()
    {
        assert(state != null);
        XState* state = (XState*)malloc(sizeof(XState));
        memcpy(state, 0, sizeof(XState));

        return state;
    }
@

Finally, we fold the name of our state block into this structure, which
completes the state block.

<<XState* updateStateStatementsWithName(char* name, XState* state);>>=

    XState* updateStateStatementsWithName(char* name, XState* state)
    {
        assert(state != null);
        state->name = name;

        return state;
    }
@

With our actions relating to state blocks out of the way, we can now define the
final structure we need for storing state blocks.  It may seem backwards to
define our data structure after defining the code that uses it, but it's often
easier to think of compilers from the bottom up, and a syntax tree data
structure is actually a fairly high-level concept.  By thinking of the problem
from the bottom-up instead of top-down, we prevent many unnecessary constraints
from being added to the design.  Don't believe me?  Try writing a compiler from
the top-down sometime, especially for a more complete and complex language than
this. :-)

<<struct XState>>=

    #define MAX_MATCH_CHARS 256

    typedef struct struct_XState
    {
        char* name;
        XMatchChars* matchChars[MAX_MATCH_CHARS];
        size_t numMatchChars;
    } XState;

    void freeXState(XState* state);
@

Additionally, we need a function that can free this structure.  This one must
also free each match clause allocated by the parser.  This concludes state block
parsing.

<<void freeXState(XState* state);>>=

    void freeXState(XState* state)
    {
        free(state->name);
        for (int i = 0; i < name->numMatchChars; ++i)
        {
            free(state->matchChars[i]);
        }

        free(state);
    }
@

Now that we have parsed both transition blocks and state blocks, we can store
them in our parse tree.  Yacc doesn't make it easy to return a syntax tree after
parsing.  While there are some bison extensions that do, I am trying to stick to
traditional Yacc for the sake of this example compiler.  Since this is a
single-threaded and very simple source-to-source compiler, we will store the
syntax tree as a global.  Since this syntax tree contains only state blocks and
transition blocks, its structure is pretty simple.

<<struct XParseTree>>=

    #define MAX_STATES 256;
    #define MAX_TRANSITIONS 512;

    struct struct_XParseTree
    {
        XState* states[MAX_STATES];
        size_t numStates;
        XTransition* transitions[MAX_TRANSITIONS];
        size_t numTransitions;
    } parseTree;
@

There's really no need for a typedef since there will only ever be one parse
tree.  On startup, main() will memset this structure, and once the parser has
completed, main() will call parseTree().  Later on, of course, command-line
options and other magic could be added.  I'll leave that as an exercise for
someone else.

<<int main(int argc, char* argv[])>>=

    int main(int argc, char* argv[])
    {
        memset(&parseTree, 0, sizeof(parseTree));

        if (yyparse() == 0)
        {
            parseTree();
        }
        else
        {
            fprintf(stderr, "Error parsing grammar.");
        }
    } 
@

Now that our parse tree is initialized, let's add our transition data to it.

<<void addTransitionDeclarationToParseTree(XTransition* transition);>>=

    void addTransitionDeclarationToParseTree(XTransition* transition)
    {
        assert(parse.numTransitions < MAX_TRANSITIONS);
        parseTree.transitions[parseTree.numTransitions] = transition;
        ++parseTree.numTransitions;
    }
@

Adding our state data is just as easy.

<<void addStateDeclarationToParseTree(XState* state);>>=

    void addStateDeclarationToParseTree(XState* state)
    {
        assert(parse.numStates < MAX_STATES);
        parseTree.states[parseTree.numStates] = states;
        ++parseTree.numStates;
    }
@

At this point, we have a complete syntax tree and all of the information we need
to generate code.

Code Generation
---------------

To generate C++ code from this parse tree, we need to do the following:

* Define a state array for storing transitions and calling a user-defined
  callback function.
* Derive this base transition class for each transition defined, using the
  provided qualified name.  This will potentially require the creation of
  namespaces.
* Define an initialization function that can be called to set the initial state.

Furthermore, we will provide a simple runtime system that will do the following:

* Define a base transition class responsible for state transitions
* Define a base state type that will contain a pointer to the current state
  array.  The user can extend this type for holding application data.  This will
  allow the state arrays and transition class instances to be re-used by
  multiple threads if so desired.
* Define a function that takes an input byte, an output stream, and a pointer
  to the current state structure, and processes the byte according to the state
  machine, calling the appropriate transitions and callbacks.  Note that the
  user-defined callbacks will be responsible for writing to the output stream,
  but this function will ensure that the right one gets called.

For now, we won't consider the problem of allowing multiple state machines in
the same application.  This can be handled later, if necessary.

The code we generate must be human-readable.  This is important if we need to
debug the code for any reason, or if we want to check to ensure that the
generated code is correct.  As we generate code, we will take certain pains to
ensure that indentation is preserved, that variable names are readable, and that
proper spacing between functions and statements is observed.  If the generated
code does not look like it was typed by a programmer, then it is not readable.

The parseTree function will open two file streams: one for the header file and
one for the source file.  It will pass these, along with an indentation pointer,
to each function it calls.


<<void parseTree();>>=

    #define HEADER_FILE_NAME "xparse-generated.h"
    #define SOURCE_FILE_NAME "xparse-generated.cpp"

    <<generateFileProlog(header, source, &headerIndentation, &sourceIndentation);>>
    <<generateStateForwardDecls(header, source, &headerIndentation, &sourceIndentation);>>
    <<generateTransitionForwardDecls(header, source, &headerIndentation, &sourceIndentation);>>
    <<generateStateArrays(header, source, &headerIndentation, &sourceIndentation);>>
    <<generateInitializationFunction(header, source, &headerIndentation, &sourceIndentation);>>
    <<generateFileEpilog(header, source, &headerIndentation, &sourceIndentation);>>

    void parseTree()
    {
        FILE* header = fopen(HEADER_FILE_NAME, "w");
        FILE* source = fopen(SOURCE_FILE_NAME, "w");
        size_t headerIndentation = 0;
        size_t sourceIndentation = 0;

        if (!header || !source)
        {
            yyerror("could not create output files."); 
            exit(1);
        }

        generateFileProlog(header, source, &headerIndentation, &sourceIndentation);
        generateStateForwardDecls(header, source, &headerIndentation, &sourceIndentation);
        generateTransitionForwardDecls(header, source, &headerIndentation, &sourceIndentation);
        generateStateArrays(header, source, &headerIndentation, &sourceIndentation);
        generateInitializationFunction(header, sourec, &headerIndentation, &sourceIndentation);
        generateFileEpilog(header, source, &headerIndentation, &sourceIndentation);

        fclose(header);
        fclose(source);
    }
@

Now, we will generate the prologues for each file.  In the header file, we will
initiate the header guard and include our runtime header.  In the source file,
we will include our header file and some basic C++ headers.


<<generateFileProlog(header, source, &headerIndentation, &sourceIndentation);>>=
    void generateFileProlog(FILE* header, FILE* source, size_t* headerIndentation, size_t* sourceIndentation)
    {
        BEGIN_HEADER_GUARD(header, HEADER_FILE_NAME, headerIndentation);
        ADD_INCLUDES(source, HEADER_FILE_NAME, sourceIndentation);
    }
@

The defines are where we stick the constants, which would take up a lot of space
in this documentation.  They are available in the appendix for reference
purposes.  To be symmetrical here, we will generate the file epilogue next.

<<generateFileEpilog(header, source, &headerIndentation, &sourceIndentation);>>=
    void generateFileEpilog(FILE* header, FILE* source, size_t* headerIndentation, size_t* sourceIndentation)
    {
        END_HEADER_GUARD(header, HEADER_FILE_NAME, headerIndentation);
    }
@

Before we can generate the forward declarations for our transition objects, we
need to think about namespaces.  A C++ namespace is a simple block.  Nested
namespaces are defined inside of an outer namespace.  Multiple namespaces
stacked up look a bit like one of those Russian dolls with a series of smaller
dolls inside.  Since our transition class can be any arbitrary nesting of
namespaces deep, we must generate a function that can iterate over the
individual components of the qualified name, and generate a set of namespaces.
Additionally, it will need to return the number of namespaces created, so that
we can end these blocks at a later time, once our forward declarations are
complete.  This number will need to be stored and used by the caller.
Back when we created the qualified name, we chose to use a period as a field
separator.  This will allow us to use strtok to split the string for iteration.

We will need to create a SplitString "class" to handle some of these details for
us.  The main reason to do an ahead-of-time iteration of the tokenized qualified
name is because we need to know the number of namespaces, which is one less than
the number of elements in the qualified name.  So, the string must be iterated
twice.  Once to tokenize and count, and once to generate namespaces.  It makes
sense to think of the split string as an object.  So... Stand back... I'm going
to write object-oriented C.

<<int buildNamespacesFromQualifiedName(FILE* file, char* qualifiedName, int* indentation);>>=
    <<struct SplitString;>>
    <<SplitString* createSplitString(char* string, char* delimiters);>>
    <<size_t SplitString_size(SplitString* ss);>>
    <<char* SplitString_get(SplitString* ss, size_t offset);>>
    <<void freeSplitString(SplitString* ss);>>

    int buildNamespacesFromQualifiedName(FILE* file, SplitString* qualifiedName, int* indentation)
    {
        int namespaceCount = SplitString_size(qualifiedName) - 1;

        for (int i = 0; i < namespaceCount; ++i)
        {
            BEGIN_NAMESPACE(file, SplitString_get(qualifiedName, i), indentation);
        }

        return namespaceCount;
    }
@

Since it will be reused, we'll go ahead and define the SplitString type and its
functions.  It won't take long...

<<struct SplitString;>>=

    #define MAX_SPLITS 256

    typedef struct struct_SplitString
    {
        char* tokenizedString;
        char* array[256];
        size_t size;
    } SplitString;
@

As before, we aren't going for general-purpose here, just good enough.  If you
are nesting namespaces more than 256 deep, then you're doing it wrong anyway.
Let's create a SplitString.  Since strtok destructively modifies the string it
is given, we will duplicate a string for tokenization and store it.

<<SplitString* createSplitString(char* string, char* delimiters);>>=

    SplitString* createSplitString(char* string, char* delimiters)
    {
        assert(string != NULL);
        SplitString* ss = (SplitString*)malloc(sizeof(SplitString));
        memset(ss, 0, sizeof(SplitString));

        ss->tokenizedString = strdup(string);
        char* token = strtok(ss->tokenizedString, delimiters);

        while (token != NULL)
        {
            ss->array[ss->size] = token;
            ++ss->size;

            if (ss->size >= MAX_SPLITS)
                return ss;

            token = strtok(NULL, delimiters);
        }

        return ss;
    }
@

The strtok algorithm will simply pass us the next string in sequence from the
tokenized buffer, so cleanup is simple.

<<void freeSplitString(SplitString* ss);>>=

    void freeSplitString(SplitString* ss)
    {
        assert(ss != NULL);
        free(ss->tokenizedString);
        free(ss);
    }
@

The size member function returns the size field, which has been set to
the number of entries found while iterating over the tokenized string.

<<size_t SplitString_size(SplitString* ss);>>=

    size_t SplitString_size(SplitString* ss)
    {
        return ss->size;
    }
@

The get member function returns the nth member, and does simple bounds checking.

<<char* SplitString_get(SplitString* ss, size_t offset);>>=

    char* SplitString_get(SplitString* ss, size_t offset)
    {
        if (offset >= ss->size)
            return NULL;

        return ss->array[offset];
    }
@

Now that we can build namespaces from a qualified name, it makes sense to build
a function for breaking out of N blocks, be they namespaces or other.  The END
BLOCK macro will fix up indentation and emit and ending curly brace, but since
this macro will be reused to end structures that might have instances, it does
not emit a newline.  Hence, we use the NEWLINE macro to close out the line.

<<void endBlocks(FILE* file, int numBlocks, int* indentation);>>=
    void endBlocks(int numBlocks, FILE* file, int* indentation)
    {
        for (int i = 0; i < numBlocks; ++i)
        {
            END_BLOCK(file, indentation);
            NEWLINE(file);
        }
    }
@

Having built these two utility functions, and the SplitString utility "class",
we can now easily implement generateTransitionForwardDecls.  This function
iterates through each defined transition block, emits the appropriate
namespaces, a class definition complete with a declaration for the on
transition function, and finally closes out each namespace.

<<generateTransitionForwardDecls(header, source, &headerIndentation, &sourceIndentation);>>=

    <<int buildNamespacesFromQualifiedName(FILE* file, char* qualifiedName, int* indentation);>>
    <<void endBlocks(FILE* file, int numBlocks, int* indentation);>>

    void generateTransitionForwardDecls(FILE* header, FILE* source, int* headerIndentation, int* sourceIndentation)
    {
        for (size_t i = 0; i < parseTree.numTransitions; ++i)
        {
            <<begin namespace>>
                <<begin class>>
                    <<build constructor>>
                    <<on transition declaration>>
                <<end class>>
            <<end namespace>>
        }
    }
@

To begin a namespace, we call the namespace function we defined above after
doing some basic data massaging to build the parameters we need.

<<begin namespace>>=
    SplitString* qualifiedName = createSplitString(parseTree.transitions[i]->usingClass;
    char* className = SplitString_get(qualifiedName, SplitString_size(qualifiedName) - 1);
    char* transitionName = parseTree.transitions[i]->name;
    char* nextState = parseTree.transitions[i]->state;

    int namespaceCount = buildNamespacesFromQualifiedName(header, qualifiedName, headerIndentation);
@

Next, we build a class which extends our transition class.  Within the public
section of this class, we will define the constructor and the on transition
function.

<<begin class>>=
    BEGIN_CLASS(className, header, headerIndentation);
        CLASS_EXTENDS(PUBLIC, CLASS_TRANSITION, header, headerIndentation);
        NEWLINE(header);

        BEGIN_BLOCK(header, headerIndentation);
            BEGIN_PUBLIC_SECTION(header, headerIndentation);
@

Our constructor will store and forward some parameters, which will be defined a
little later in the document.  For now, we will capture this in macro form.

<<build constructor>>=
    BEGIN_CONSTRUCTOR(className, TRANSITION_CTOR_ARGS, header, headerIndentation);
        BEGIN_CTR_ARG_ASSIGNS(header, headerIndentation);
            EMIT_TRANSITION_CTOR_ARGS(nextState, header, headerIndentation);
        END_CTOR_ARG_ASSIGNS(header, headerIndentation);
        BEGIN_BLOCK(header, headerIndentation);
        END_BLOCK(header, headerIndentation);
        NEWLINE(header);
    END_CONSTRUCTOR(className, header, headerIndentation);
@

We also need to emit a declaration for the emit transition function.  The user
will provide a definition for this function, which will perform any work we need
done while transitioning from state to state.  More on this later.

<<on transition declaration>>=
    EMIT_TRANSITION_FUNCTION_ON_TRANSITION(header, headerIndentation);
@

Finally, we end the class.

<<end class>>=
            END_PUBLIC_SECTION(header, headerIndentation);
        END_BLOCK(header, headerIndentation);
    END_CLASS_WITH_INSTANCE(transitionName, header, headerIndentation);
@

Then, we end the namespace.

<<end namespace>>=
    endBlocks(namespaceCount, header, headerIndentation);
@

With that, all transition classes have been emitted.  We are half-way done with
code generation.  Next, we must consider how to emit state blocks.  There are
two pieces here.  The first is to set up forward declarations for state arrays.
Once this is done, then we will emit the actual state arrays in the C++ source
file.

The first step is to emit the forward declarations for our state arrays.  These
are used by the transition class instances to transition to the next state.  We
will declare each one extern; the implementations will be placed in the source
file later.

<<generateStateForwardDecls(header, source, &headerIndentation, &sourceIndentation);>>=
    void generateStateForwardDecls(FILE* header, FILE* source, int* headerIndentation, int* sourceIndentation)
    {
        for (size_t i = 0; i < parseTree.numStates; ++i)
        {
            EXTERN(header, headerIndentation);
                INSTANCE(STATE_TYPE, parseTree.states[i]->name, header, headerIndentation);
                END_STATEMENT(header, headerIndentation);
                NEWLINE(header, headerIndentation);

            //add a newline in between to enhance readability
            NEWLINE(header, headerIndentation);
        }
    }
@

Now, we can talk about generating the state arrays in the source file.  Each
state array has 256 entries, representing each possible byte value.  Each entry
is a pointer to the transition that is used when a particular byte is
encountered in this state.  In order to enhance readability, we will emit one
entry per line, and also emit a line above this entry representing the actual
byte value used for this line.  Each set of lines will look like this:

    /* 0x41  65  'A' */
    &ignore,

Here, we can see the hex representation of the byte, the decimal
representation, and the printable glyph if applicable.  The function to generate
this comment is here:

<<void generateTransitionComment(unsigned char ch, FILE* file, int* indentation);>>=
    void generateTransitionComment(unsigned char ch, FILE* file, int* indentation)
    {
        BEGIN_COMMENT(file, indentation);
            HEX_VALUE(ch, file, indentation);
            DECIMAL_VALUE(ch, file, indentation);
            if (isprint(ch) && !(ch & 0x80))
                PRINT_VALUE(ch, file, indentation);
        END_COMMENT(file, indentation);
        NEWLINE(file, indentation);
    }
@

Now, we need to take the XMatchChars structure that we built up during parsing,
and turn this into an array of transition names.  We also need to check for two
error conditions: the lack of a default character class, and duplicate matching
characters for different transitions.  The function is actually pretty simple.
We'll make two passes through the XState instance.  The first time through, we
will populate all match chars instances.  The second time through, we will
populate the remaining members of the array with the default character class.

<<char** buildCharacterArray(XState* state);>>=
    char** buildCharacterArray(XState* state)
    {
        char** array = (char**)malloc(256*sizeof(char*));
        memset(array, 0, 256*sizeof(char*));
        MatchChars* defaultMatch = NULL;

        <<match chars loop>>
        <<default chars loop>>

        return array;
    }
@

Here is the first loop through the match chars.  We skip the default character
class if we find it, but preserve it for later.  If we come across it twice, we
report an error.  Likewise, if we see that an entry in the array has been set
twice, we report an error.

<<match chars loop>>=
    for (int i = 0; i < state->numMatchChars; ++i)
    {
        XMatchChars* match = state->matchChars[i];
        if (match->type == E_DEFAULT)
        {
            if (defaultMatch)
            {
                fprintf(stdout, "Error, :default: defined multiple times for %s\n", state->name);
                exit(1);
            }

            defaultMatch = match;
            continue;
        }

        for (int j = 0; j < match->charactersSize; ++j)
        {
            if (array[match->characters[j]])
            {
                sprintf(stderr, "Error in %s: Character %c already defined for transition %s\n",
                        array[match->characters[j]], match->characters[j], match->transitionName);
                exit(1);
            }

            array[match->characters[j]] = match->transitionName;
        }
    }
@

The second time through, we'll iterate over the array and fill out any missing
entries with the default character class.  This will complete the array.

<<default chars loop>>=
    if (!defaultMatch)
    {
        fprintf("Error: missing :default: clause in state %s\n", state->name);
        exit(1);
    }

    for (int i = 0; i < 256; ++i)
    {
        if (!arrays[i])
        {
            arrays[i] = defaultMatch->transitionName;
        }
    }
@

Now, we can implement the code to generate state arrays.  We'll build a state
array instance, build a character array of transitions, and emit each transition to
the array.  Also, we'll add comments to make the entries readable.

<<generateStateArrays(header, source, &headerIndentation, &sourceIndentation);>>=

    <<void generateTransitionComment(unsigned char ch, FILE* file, int* indentation);>>
    <<char** buildCharacterArray(XState* state);>>

    void generateStateArrays(FILE* header, FILE* source, int* headerIndentation, int* sourceIndentation)
    {
        for (int i = 0; i < parseTree.numStates; ++i)
        {
            INSTANCE(STATE_TYPE, parseTree.states[i]->name, source, sourceIndentation);
            ASSIGNMENT(source, sourceIndentation);
            BEGIN_BLOCK(source, sourceIndentation);

            char** array = buildCharacterArray(parseTree.states[i]);
            for (int j = 0; j < 256; ++j)
            {
                generateTransitionComment((unsigned char)j, source, sourceIndentation);
                POINTER_REFERENCE(array[j], source, sourceIndentation);
            }
            free(array);

            END_BLOCK(source, sourceIndentation);
            END_STATEMENT(source, sourceIndentation);

            NEWLINE(source, sourceIndentation);
            NEWLINE(source, sourceIndentation);
        }
    }
@

The last piece to consider for code generation, other than the macros in the
appendix that write out the C++ code, is our initialization function.
This will take an application state object, and set its state to our initial
state.

<<generateInitializationFunction(header, source, &headerIndentation, &sourceIndentation);>>=
    void generateInitializationFunction(FILE* header, FILE* source, int* headerIndentation, int* sourceIndentation)
    {
        char statement[1024];
        if (parseTree.numStates < 1)
        {
            fprintf(stderr, "Error: expecting at least one state.\n");
            exit(1);
        }
        char* initialState = parseTree.states[0]->name;

        FUNCTION_DECL(INIT_FUNCTION, "void", "ApplicationState& state", header, headerIndentation);
        NEWLINE(header, headerIndentation);

        FUNCTION(INIT_FUNCTION, "void", "ApplicationState& state", source, sourceIndentation);
        NEWLINE(source, sourceIndentation);
        BEGIN_BLOCK(source, sourceIndentation);
        sprintf(statement, "state.state = &%s;", initialState);
        EMIT_STATEMENT(statement, source, sourceIndentation);
        END_BLOCK(source, sourceIndentation);
        NEWLINE(source, sourceIndentation);
        NEWLINE(source, sourceIndentation);
    }
@

This concludes our section on code generation.

Runtime
-------

Now, we can describe the runtime system.  This consists of any code that is not
generated by the compiler, but is necessary in order to run the application.  We
also include definitions on which the generated code depends, such as the State
class or the ApplicationState base object.  The runtime library includes the
filterByte function that we described earlier in the document.  This is the
easiest place to start.

<<filterByte(unsigned char byte, ApplicationState& state, std::ostream& out);>>=
    EStatus filterByte(unsigned char byte, ApplicationState& state, std::ostream& out)
    {
        Transition* t = (*state.state)[byte];
        return t->transition(byte, state, out);
    }
@

This is the simplicity that the state machine gives us.  All filtering of input
data, which now consists of just a luhn check, but could easily be expanded
later on to handle arbitrary sensitive data, takes about two lines of code in
this function.  We will see a similar pattern when we examine the rest of the
runtime.

This function uses an ApplicationState object.  This is an object that can be
overridden by the user to store additional application state, such as the luhn
check buffers.  It is described here.

<<struct ApplicationState;>>=
    typedef
    Transition TransitionArray[256];

    struct ApplicationState
    {
        TransitionArray* state;
    };
@

The base Transition type is overridden by the compiler to provide specific
instance of onTransition for the user to implement.  The transition function
simply calls onTransition, and then sets the next state for the state machine.
The mechanism of the state machine is completely encapsulated away from user
code, which simply has to deal with specific operations around bytes and
streams.

<<class Transition;>>=
    class Transition
    {
    public:
        Transition(TransitionArray* state)
            : state_(state)
        {
        }

        EStatus transition(unsigned char byte, ApplicationState& state, std::ostream& out);

    protected:
        virtual EStatus onTransition(unsigned char byte, ApplicationState& state, std::ostream& out) const = 0;
    };
@

Here we define the transition function.  It is also deceptively simple.

<<Transition::transition(unsigned char byte, ApplicationState& state, std::ostream& out);>>=
    inline EStatus Transition::transition(unsigned char byte, ApplicationState& state, std::ostream& out)
    {
        state.state = state_;
        return onTransition(byte, state, out);
    }
@

The rest of the runtime is source file / header boilerplate, which is available
in the appendix.  This concludes our description of the state machine.  We can
now focus on the Luhn Check algorithm itself.  :-)

Appendix
========

This appendix completes the code base with code that is necessary for the
compiler, but is not part of the main discussion.  Here, we have boilerplate
code and some C++ code generation macros.

Yacc Header
-----------

This is the yacc header boilerplate, which is required for compiling the parser.
It's largely uninteresting for our discussion.  Main is elided, because it is
covered in the documentation up top.

<<yacc header>>=

    %{
        #include <stdio.h>
        #include <string.h>
        #include "xparse.h"

        void yyerror(const char *str)
        {
            fprintf(stderr,"error: %s\n",str);
        }

        int yywrap()
        {
            return 1;
        } 

        <<int main(int argc, char* argv[])>>
    %}

@

xparse.c definition
-------------------

<<xparse.c>>=

    #include "xparse.h"

    <<code generation macros>>
    <<struct XParseTree>>

    <<void parseTree();>>
    <<void addStateDeclarationToParseTree(XState* state);>>
    <<void addTransitionDeclarationToParseTree(XTransition* transition);>>

    <<XState* updateStateStatementsWithName(char* name, XState* state);>>
    <<XState* createEmptyStateStatements();>>
    <<XState* foldMatchClauseIntoStateStatements(XState* state, XMatchChars* matchClause);>>
    <<XMatchChars* createMatch(char* matchStmt, char* stateTransition);>>
    <<char* createTransitionStatement(char* transitionName);>>
    <<char* createMatchChars(char* match_chars);>>
    <<char* createDefaultMatchClass();>>

    <<XTransition* createTransition(char* name, XTransition* transitionStatements);>>
    <<XTransition* createTransitionStruct();>>
    <<XTransition* createUsingClass(XTransition* transition, char* usingClassDecl);>>
    <<XTransition* createStateAssignment(XTransition* transition, char* stateAssignment);>>
    <<char* createQualifiedClassName(char* className, char* identifier);>>
    <<char* createSetState(char* state);>>
    <<void freeXTransition(XTransition* transition);>>
    <<void freeXMatchChar(XMatchChars* match);>>
    <<void freeXState(XState* state);>>
@

Code generation constants and macros
------------------------------------

<<code generation macros>>=
    <<#define NEWLINE>>
    <<#define BEGIN_HEADER_GUARD>>
    <<#define END_HEADER_GUARD>>
    <<#define ADD_INCLUDES>>
    <<#define BEGIN_BLOCK>>
    <<#define END_BLOCK>>
    <<#define BEGIN_NAMESPACE>>
    <<#define BEGIN_CLASS>>
    <<#define CLASS_EXTENDS>>
    <<#define BEGIN_PUBLIC_SECTION>>
    <<#define BEGIN_CONSTRUCTOR>>
    <<#define BEGIN_CTR_ARG_ASSIGNS>>
    <<#define EMIT_TRANSITION_CTOR_ARGS>>
    <<#define END_CTOR_ARG_ASSIGNS>>
    <<#define END_CONSTRUCTOR>>
    <<#define EMIT_TRANSITION_FUNCTION_ON_TRANSITION>>
    <<#define END_PUBLIC_SECTION>>
    <<#define END_BLOCK>>
    <<#define END_CLASS>>
    <<#define END_CLASS_WITH_INSTANCE>>
    <<#define EXTERN>>
    <<#define INSTANCE>>
    <<#define BEGIN_COMMENT>>
    <<#define END_COMMENT>>
    <<#define HEX_VALUE>>
    <<#define DECIMAL_VALUE>>
    <<#define PRINT_VALUE>>
    <<#define END_STATEMENT>>
@

[flex_website]: http://flex.sourceforge.net/ "flex: The Fast Lexical Analyzer"
[yacc_left_recursion]: http://tldp.org/HOWTO/Lex-YACC-HOWTO-6.html#ss6.2 "Recursion: right is wrong."
