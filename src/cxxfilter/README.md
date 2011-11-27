C++ Solution to the Square LuhnyBin Problem
===========================================

This solution uses a simple table based parser with two states to scan an input
stream and write to a scrubbed output stream.

Introduction
------------

Balancing the complexity of a solution to this problem with readability is was
one of my concerns.  I firmly believe that software isn't meant to be
comprehended by a computer. That's incidental. Humans are the true audience of
software, and it should always be written with this in mind.

That being said, writing a solution that is human-readable can be a daunting
task.  Given this, I decided to use the literate style of programming.  Yes,
you are reading the source code to this solution right now.  I am using
[noweb][noweb_website] and [Markdown][markdown_wiki]; the former because it is
flexible enough to tangle arbitrary source code and arbitrary documentation
formats, and the latter because it keeps to the spirit of readability.

This example may be a bit over the top, but I wanted to have a little fun while
solving this problem.  Bear that in mind when examining some of the design
decisions.  The software is serious where it counts, but certainly, writing a
DSL to handle text parsing is a bit silly.  In my free time, I contend that this
is perfectly allowable.  :-)

Even among the silly, there are some interesting design considerations here.
First, the combination of Markdown with noweb does make for an almost viable
literate programming framework.  Let's face it, documentation is generally not
something that comes easily when writing software.  Anything that can improve
documentation, especially by allowing stream-of-consciousness design to be
documented as one thinks through the problem, is invaluable.  Here, software
becomes almost like a conversation in which I describe the problem, explain what
I am going to do, and then show snippets of code as I write it.  Compared to
trying to do a source dive in an under-documented code base, or even worse, a
code base with woefully outdated documentation, this has to be a better
solution.  Well, if you can bear my long-windedness.  :-)

I still have not figured out how to incorporate test-driven development into the
prose of literate programming.  My tests are in a companion document.

Building
--------

In order to build this solution, you will need the following tools installed.
Most of these should already be available in a standard Unix installation.  If
you are using Mac OS X, or Windows (via [cygwin][cygwin_website]), then you
will need to install these using the appropriate setup tools.

* [GNU Make][gnumake_website]
* A C++ compiler.  (Please set CXX, CXXFLAGS, and LDFLAGS appropriately)
  [gcc][gcc_website] is an excellent choice here.

Make sure that all required tools are available in the path.  From the
luhnybin/src/ directory, run gmake.  If there are any issues with the
build process, please contact me (nanolith+luhnybin@gmail.com) with details.
Please include the platform you are using and the specific versions of tools
that your are running.

Once built, you can execute the run.sh command as expected.

Niceties
--------

Since this document combines both Markdown and source code, proper syntax
highlighting is really required for both.  I can't provide syntax highlighting
for everyone's favorite IDEs, but I can provide [vim][vim_website] syntax files.

To use these files, copy the directory structure provided in luhnybin/vim to
your own ~/.vim/ directory.  Note that the location of your vim 
directory may vary depending upon your operating system choice.

Software Description
====================

The first goal in building software should be to understand the problem.  In
addition to the [original problem description][square_luhn], I wanted to
describe what would make for a general solution.  What we are looking to build
is a simple filter that, given an input stream and an output stream, will mask
any sequence of ([0-9]|-| ) with Xes.  The goal is to perform this as
efficiently as possible, while not sacrificing the flexibility of stream I/O.
While the solution provided here works as expected with the original problem
description, it could be easily extended to take advantage of
[Boost.Iostreams][boost_iostreams] and even be folded into a general purposes
logging library.

In order to best describe this parser, we will write a state machine
description, which is provided below.  This state machine will be transformed to
our target language, thus allowing us to focus on documenting exactly what we
want the parser to do, and then we can focus on turning this description into
code that performs this task.  Additionally, the state machine description
below could be ported to languages other than C++ by using one of the various
DSL composition systems available.  In our example, we will use
[flex][flex_website] and [bison][bison_website].  (clones of [lex][lex_wiki]
and [yacc][yacc_wiki]).

The code to transform this state machine description into C++ code is provided
in the companion document, [Parsing the State Machine][parse_document].

Assumptions
-----------

As with any software, we must make some assumptions about acceptable practice.
The first assumption I have made is that the log streams will be using either
ASCII or [UTF-8][utf8_wiki].  My parser must support both.  Thankfully,
supporting UTF-8 in C++ parser like this is actually pretty simple.  As long as
I operate on raw bytes and keep input and output sane, I can largely ignore
multibyte glyphs.

State Machine
-------------

The state machine is shown below.

    <<luhncheck.xparse>>=

    state initial {
        [0-9] { transition->luhnGather }
        :default: { transition->ignore }
    }

    transition ignore {
        using class transition.Ignore
        state->initial
    }

    transition luhnGather {
        using class transition.LuhnGather
        state->luhn
    }

    state luhn {
        [0-9]     { transition->luhnAppendDigit }
        [ \-]     { transition->luhnAppendNonDigit }
        :default: { transition->luhnCheck }
    }

    transition luhnAppendDigit {
        using class transition.LuhnAppendDigit
        state->luhn
    }

    transition luhnAppendNonDigit {
        using class transition.LuhnAppendNonDigit
        state->luhn
    }

    transition luhnCheck {
        using class transition.LuhnCheck
        state->initial
    }
    @

To read the state machine, assume that it is a simple cyclic graph.  The states
are the nodes, and the transitions are the edges.  In this state machine, there
are two states: the initial state, in which any byte except 0-9 are simply
copied to standard output via the ignore transition, and luhn, in which we
will first gather, and then Luhn Check, digits.  In this state, there are three
transitions.

1. luhnAppendDigit, which appends a digit to our check buffer.
   This transition goes back to the luhn state.
2. luhnAppendNonDigit, which appends a non-digit to our check buffer.
   This transition goes back to the luhn state.
3. luhnCheck, which performs a Luhn Check of the saved digits.
   Any sequence of 14, 15, or 16 digits is greedily transposed to Xes.
   This transition goes to the initial state, thus resetting the parser.

The reason why we use a "using class" declaration above is so we can apply an
object-oriented decomposition of state transitions, which cleans up the
generated code.

Luhn Checking
=============

Our state machine has defined two states and several transitions that we will
use for filtering a log file.  The nice thing about this state machine is that
our luhn logic can be written in a much simpler way without dealing with the
concerns of filtering and checking input streams for byte sequences.  All of
this lifting has been done for us by the state machine.  Here are the cases with
which we have to concern ourselves: writing non-luhn bytes to the output file,
setting up our luhn check state when we first encounter digits, checking
these digits for luhn via fast fail logic, dumping non-luhn digits, and
replacing Luhn sequences.  Here is the skeleton of our source file.

    <<cxxfilter.cpp>>=
    #include <xparse_runtime.h>
    #include <xparse_generated.h>

    <<struct LuhnState;>>

    <<transition::Ignore::onTransition(unsigned char byte, ApplicationState& state, std::ostream& out);>>
    <<transition::LuhnGather::onTransition(unsigned char byte, ApplicationState& state, std::ostream& out);>>
    <<transition::LuhnAppendDigit::onTransition(unsigned char byte, ApplicationState& state, std::ostream& out);>>
    <<transition::LuhnAppendNonDigit::onTransition(unsigned char byte, ApplicationState& state, std::ostream& out);>>
    <<transition::LuhnCheck::onTransition(unsigned char byte, ApplicationState& state, std::ostream& out);>>

    <<int main(int argc, char* argv[]);>>
    @

Our base case, in which we just write non-Luhn bytes to the output stream, is by
far the easiest.

    <<transition::Ignore::onTransition(unsigned char byte, ApplicationState& state, std::ostream& out);>>=
    EStatus transition::Ignore::onTransition(unsigned char byte, ApplicationState& state, std::ostream& out) const
    {
        out.write((const char*)&byte, sizeof(byte));

        return E_SUCCESS;
    }
    @

When we first encounter a digit, we enter the luhn state via the LuhnGather
transition.  Here, we want to reset our internal state, and append this digit
to our luhn buffer.

    <<transition::LuhnGather::onTransition(unsigned char byte, ApplicationState& state, std::ostream& out);>>=
    EStatus transition::LuhnGather::onTransition(unsigned char byte, ApplicationState& state, std::ostream& out) const
    {
        LuhnState& luhn = (LuhnState&)state;
        luhn.reset();
        luhn.appendDigit(byte);

        return E_SUCCESS;
    }
    @

The LuhnAppendDigit and LuhnAppendNonDigit transitions are also
straight-forward.  Here, we just append either a digit or non-digit.  The
distinction comes down to our digit counts, which allow us to fail fast in cases
where the digit sequence is less than the minimum of 14 digits.

    <<transition::LuhnAppendDigit::onTransition(unsigned char byte, ApplicationState& state, std::ostream& out);>>=
    EStatus transition::LuhnAppendDigit::onTransition(unsigned char byte, ApplicationState& state, std::ostream& out) const
    {
        LuhnState& luhn = (LuhnState&)state;
        luhn.appendDigit(byte);

        return E_SUCCESS;
    }
    @

Here is the non-digit form.

    <<transition::LuhnAppendNonDigit::onTransition(unsigned char byte, ApplicationState& state, std::ostream& out);>>=
    EStatus transition::LuhnAppendNonDigit::onTransition(unsigned char byte, ApplicationState& state, std::ostream& out) const
    {
        LuhnState& luhn = (LuhnState&)state;
        luhn.appendNonDigit(byte);

        return E_SUCCESS;
    }
    @

Finally, we can perform the luhn checking transition.  We will encapsulate the
luhn checking logic away from this transition for simplicity sake.  We are
expecting the luhn check to process the sequence from right to left, find all
subsequences that pass a luhn check and are at least 14 digits long, and scrub
these from the buffer.  If *any* luhn sequence is found, then it will return a
status of E LUHN, which we will pass back to the caller.  It is the caller's
responsibility to call the calvary or do whatever else must be done.

    <<transition::LuhnCheck::onTransition(unsigned char byte, ApplicationState& state, std::ostream& out);>>=
    EStatus transition::LuhnCheck::onTransition(unsigned char byte, ApplicationState& state, std::ostream& out) const
    {
        LuhnState& luhn = (LuhnState&)state;
        EStatus status = luhn.check();

        //write the buffered, possibly scrubbed, sequence
        out.write(luhn.bytes(), luhn.size());
        //write the sequence breaking byte
        out.write((const char*)&byte, sizeof(byte));

        return status;
    }
    @

Now, we can describe the LuhnState object, which extends ApplicationState.  It
contains a vector to hold the bytes that we are gathering for our Luhn check,
and a count of digits encountered.

    <<struct LuhnState;>>=
    struct LuhnState : public ApplicationState
    {
        <<LuhnState public typedefs>>

        LuhnState();

        void appendDigit(unsigned char digit);
        void appendNonDigit(unsigned char byte);
        void reset();
        EStatus check();
    
	    const char* bytes();
    	size_t size();

    private:
        ByteBuffer buffer;
        size_t count;

        void findLuhnSequence(reverse_iterator i, reverse_iterator rend, IteratorPairsList& luhnList);
        void scrubLuhnDigits(iterator begin, iterator end);
        reverse_iterator first_digit(reverse_iterator begin, reverse_iterator end);
    };

    <<LuhnState::LuhnState();>>
    <<void LuhnState::appendDigit(unsigned char digit);>>
    <<void LuhnState::appendNonDigit(unsigned char byte);>>
    <<void LuhnState::reset();>>
    <<EStatus LuhnState::check();>>
    <<void LuhnState::findLuhnSequence(reverse_iterator i, reverse_iterator rend, IteratorPairsList& luhnList);>>
    <<void LuhnState::scrubLuhnDigits(iterator begin, iterator end);>>
    <<reverse_iterator LuhnState::first_digit(reverse_iterator begin, reverse_iterator end);>>
    <<const char* LuhnState::bytes();>>
    <<size_t LuhnState::size();>>
    @

In this state class, we have a few typedefs to make life easier.  This includes
the buffer type in which we will be staging bytes, and some containers we will
be using for Luhn checking.  They are defined here.

    <<LuhnState public typedefs>>=
    typedef
    std::vector<unsigned char>
    ByteBuffer;

    typedef
    std::pair<ByteBuffer::iterator, ByteBuffer::iterator>
    IteratorPairs;

    typedef
    std::list<IteratorPairs>
    IteratorPairsList;

    typedef
    ByteBuffer::iterator
    iterator;

    typedef
    ByteBuffer::reverse_iterator
    reverse_iterator;
    @

The LuhnState constructor will reserve a decent size for the buffer to reduce
allocation calls.  It will also call reset() to set the count to 0.

    <<LuhnState::LuhnState();>>=
    LuhnState::LuhnState()
    {
        buffer.reserve(2048);
        reset();
    }
    @

Here is reset.  Clears the buffer and sets count to 0.

    <<void LuhnState::reset();>>=
    void LuhnState::reset()
    {
        buffer.clear();
        count = 0;
    }
    @

AppendNonDigit just appends the byte to the buffer.

    <<void LuhnState::appendNonDigit(unsigned char byte);>>=
    void LuhnState::appendNonDigit(unsigned char byte)
    {
        buffer.push_back(byte);
    }
    @

AppendDigit increments the count and then calls appendNonDigit.

    <<void LuhnState::appendDigit(unsigned char digit);>>=
    void LuhnState::appendDigit(unsigned char digit)
    {
        ++count;
        appendNonDigit(digit);
    }
    @

This leaves the luhn check logic itself.  We know at this point that we have a
buffer containing characters, some of which are digits, and a count of digits in
this buffer.  We will first see if the count of digits is less than the minimum,
and if so, bypass the test.  Next, we will iterate from right to left, and
greedy match the largest luhn sequence we can find that is between 14 and 16
digits inclusive.  The set of iterators marking the begin and end of this
sequence will be appended to a linked list for later processing.  This allows us
to continue matching all the way until the end of the buffer to find adjacent or
even nested matches without masking bytes in between.  If any matches are found,
we iterate through the list scrubbing all digits, and then return an E LUHN
status code.

    <<EStatus LuhnState::check();>>=
    #define MINIMUM_LUHN 14
    #define MAXIMUM_LUHN 16

    EStatus LuhnState::check()
    {
        if (count < MINIMUM_LUHN)
            return E_SUCCESS;

        EStatus result = E_SUCCESS;

        <<build luhn sequence list>>
        <<scrub luhn sequences>>

        return result;
    }
    @

The Luhn sequence list is a list of pairs of iterators representing the
beginning and end of a Luhn sequence.  We use a reverse iterator to go through
the buffer from right to left, adding matching Luhn sequences to this list.

    <<build luhn sequence list>>=
    IteratorPairsList luhnList;

    ByteBuffer::reverse_iterator rend = buffer.rend();
    for (ByteBuffer::reverse_iterator i = buffer.rbegin();
        i != rend; ++i)
    {
        findLuhnSequence(i, rend, luhnList);
    }
    @

If our list is not empty, then we will set the returned status to E LUHN, and
proceed to scrub each entry in the list.

    <<scrub luhn sequences>>=
    if (!luhnList.empty())
        result = E_LUHN;

    for (IteratorPairsList::iterator i = luhnList.begin();
        i != luhnList.end(); ++i)
    {
        scrubLuhnDigits(i->first, i->second);
    }
    @

Now, we can focus on the code that will find a Luhn sequence from right to left.
We want to perform a greedy match, meaning that we want to find the largest
sequence of digits, within the given constraints, that pass the Luhn check.  If
such a sequence is found, we will append this to the list.  Note the flipping of
second and first when appending to the list.  We do this because we want to
convert from reverse iterators to iterators.  This logic turns second into the
beginning of the sequence, and first into the exclusive end of the sequence.

    <<void LuhnState::findLuhnSequence(reverse_iterator i, reverse_iterator rend, IteratorPairsList& luhnList);>>=
    void LuhnState::findLuhnSequence(reverse_iterator i, reverse_iterator rend, IteratorPairsList& luhnList)
    {
        <<set initial state>>

        for (i = first; i != rend && digits < MAXIMUM_LUHN; i = first_digit(++i, rend))
        {
            <<luhn sum>>
            <<check sequence>>
        }

        if (resultFound)
        {
            luhnList.push_back(make_pair((second+1).base(), first.base()));
        }
    }
    @

Here, we set the initial state.  Our first iterator is set to the first digit in
the sequence, or rend if no digits are found.  Our number of digits is 0, parity
is set to false, resultFound is set to false, and sum is set to 0.

    <<set initial state>>=
    reverse_iterator first = first_digit(i, rend);
    reverse_iterator second;
    int digits = 0;
    bool parity = false;
    bool resultFound = false;
    int sum = 0;
    @

For each digit found, we will compute the value of the digit, and add this value
to our sum.  The parity flag is used for determining whether we add the value
or double the value.  Finally, we increment the number of digits and flip the
parity bit for the next run.

    <<luhn sum>>=
    int val = *i - '0';
    if (parity) val <<= 1;

    if (val >= 10)
        val = val%10 + val/10;

    sum += val;

    ++digits;
    parity^=true;
    @

Our loop terminates when either the end of the buffer is encountered, or when we
have found more than MAXIMUM LUHN digits.  This allows us to apply the following
check after we have found at least MINIMUM LUHN digits.  This check does a mod
10 of our sum, and if this results in 0, we have a set of digits that pass the
luhn check.  We will set our resultFound flag, set the second iterator to the
end of this sequence and continue.  By continuing, we are performing a greedy
match.  Note that we aren't setting the iterator to the exclusive end of the
sequence.  This is because we have to convert these iterators back to regular
iterators when we add them to our list.  First becomes the end iterator, and
second becomes the begin iterator.

    <<check sequence>>=
    if (digits >= MINIMUM_LUHN)
    {
        if (sum%10 == 0)
        {
            resultFound = true;
            second = i;
        }
    }
    @

The last thing we need for this algorithm to work is a way to scan to the next
digit.  This is provided by the first digit function, which, given a pair of
iterators, will return either an iterator to the first digit, or the end of the
sequence if a digit is not found.  This function allows us to skip non-digits in
a sequence efficiently.

    <<reverse_iterator LuhnState::first_digit(reverse_iterator begin, reverse_iterator end);>>=
    LuhnState::reverse_iterator LuhnState::first_digit(LuhnState::reverse_iterator begin, LuhnState::reverse_iterator end)
    {
        for (; begin != end; ++begin)
        {
            if (isdigit(*begin))
                return begin;
        }

        return begin;
    }
    @

Once we have found Luhn sequences, we need to scrub them from our buffer.  This
is handled by the scrubLuhnDigits.  For each byte in the sequence, if we
encounter a digit, change it to our scrub byte.

    <<void LuhnState::scrubLuhnDigits(iterator begin, iterator end);>>=
    #define SCRUB_BYTE 'X'

    void LuhnState::scrubLuhnDigits(iterator begin, iterator end)
    {
        for (; begin != end; ++begin)
        {
            if (isdigit(*begin))
                *begin = SCRUB_BYTE;
        }
    }
    @

This concludes our Luhn checking logic.  There are two final functions needed
for LuhnState.  The first returns a pointer to the buffer for use by
ostream::write.  Since STL vector stores data in contiguous memory, this
operation works.  It is actually mentioned in the C++ standard as safe.

    <<const char* LuhnState::bytes();>>=
    const char* LuhnState::bytes()
    {
        return (const char*)&buffer[0];
    }
    @

The second function returns the current size of the buffer.

    <<size_t LuhnState::size();>>=
    size_t LuhnState::size()
    {
        return buffer.size();
    }
    @

cxxfilter application
---------------------

The cxxfilter application copies standard input to standard output, using the
filtering logic that we have built here.  First, it calls the generated
function, initializeApplicationState, which sets up our state machine.  Then, it
loops on standard input, reading characters and passing these to our filter.
This is the main routine:

    <<int main(int argc, char* argv[]);>>=
    using namespace std;

    int main(int argc, char* argv[])
    {
        LuhnState applicationState;

        initializeApplicationState(applicationState);

        while (!cin.eof())
        {
            unsigned char ch;
            cin.read((char*)&ch, sizeof(ch));

            if (cin.eof())
                return 0;

            if (E_LUHN == filterByte(ch, applicationState, cout))
            {
                //TODO: call the calvary
            }
        }
    }
    @

[noweb_website]: http://www.cs.tufts.edu/~nr/noweb/ "Noweb home page"
[markdown_wiki]: http://en.wikipedia.org/wiki/Markdown "Markdown"
[gnumake_website]: http://www.gnu.org/s/make/ "GNU Make"
[gnutar_website]: http://www.gnu.org/software/tar/ "GNU Tar"
[gzip_website]: http://www.gzip.org/ "The gzip homepage"
[gcc_website]: http://gcc.gnu.org/ "GCC, the GNU Compiler Collection"
[curl_website]: http://curl.haxx.se/ "cURL"
[cygwin_website]: http://www.cygwin.com/ "Cygwin"
[boost_iostreams]: http://www.boost.org/doc/libs/1_48_0/libs/iostreams/doc/index.html "Boost.Iostreams"
[vim_website]: http://www.vim.org/ "vim online"
[utf8_wiki]: http://en.wikipedia.org/wiki/UTF-8 "UTF-8"
[flex_website]: http://flex.sourceforge.net/ "flex: The Fast Lexical Analyzer"
[bison_website]: http://www.gnu.org/s/bison/ "Bison - GNU parser generator"
[yacc_left_recursion]: http://tldp.org/HOWTO/Lex-YACC-HOWTO-6.html#ss6.2 "Recursion: right is wrong."
[square_luhn]: http://corner.squareup.com/2011/11/luhny-bin.html "Coding Challenge: The Luhny Bin"
[lex_wiki]: http://en.wikipedia.org/wiki/Lex_%28software%29 "lex (software)"
[yacc_wiki]: http://en.wikipedia.org/wiki/Yacc "yacc"
[parse_document]: PARSER.md "Parsing the State Machine"
[test_document]: TDD.md "Testing the Luhn Check Algorithm"
