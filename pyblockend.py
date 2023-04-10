#!/usr/bin/env python3

import re
import sys
from enum import Enum
from collections import namedtuple
from functools import reduce

# -----------------------------------------------------------------------------

DEFAULT_TAB_WIDTH = 4
TAB_WIDTH = DEFAULT_TAB_WIDTH

# -----------------------------------------------------------------------------


class String(str):
    def __init__(self, *args, **kwargs):
        if len(args) and isinstance(args[0], String):
            self.name = args[0].name
            self.lnum = args[0].lnum
            self.lpos = args[0].lpos
        else:
            self.name = None
            self.lnum = None
            self.lpos = None

    def __repr__(self):
        return f'(String:{repr(self.name)}:{self.lnum}:{self.lpos}:{repr(super())})'

    def __add__(self, rhs):
        return String.create(self.name, self.lnum, self.lpos, super().__add__(rhs))

    def setparam(self, name, lnum, lpos):
        self.name = name
        self.lnum = lnum
        self.lpos = lpos
        return self

    @staticmethod
    def create(name, lnum, lpos, s=''):
        return String(s).setparam(name, lnum, lpos)


class ReadStream:
    def __init__(self, name, stream=None):
        self.name = name if name != '-' else '[STDIN]'
        self.stream = (stream if stream else
                       (sys.stdin if name == '-' else open(name)))
        self.lines = []
        self.line = ''
        self.lnum = 0
        self.lpos = 0
        self.pending = []

    def getline(self):
        line = self.stream.readline()
        if line:
            self.lines.append(line)
            self.line = line
            self.lnum += 1
            self.lpos = 0
        return line

    def getchar(self):
        if self.pending:
            return self.pending.pop()
        if self.lpos >= len(self.line):
            if not self.getline():
                return None
        lpos = self.lpos
        self.lpos = lpos + 1
        return String.create(self.name, self.lnum, lpos, self.line[lpos])

    def putchar(self, c):
        self.pending.append(c)
        return self

    def read(self, n):
        t = tuple(self.getchar() for _ in range(n))
        r = (''.join(s or '' for s in t), t)
        return r

    def write(self, s):
        self.pending += reversed(s)
        return self

    def emptychar(self):
        return String.create(self.name, self.lnum, self.lpos, '')


class WriteStream:
    def __init__(self, name, stream=None):
        self.name = name if name != '-' else '[STDOUT]'
        self.stream = (stream if stream else
                       (sys.stdout if name == '-' else open(name, 'w')))

    def write(self, data):
        return self.stream.write(data)


# -----------------------------------------------------------------------------


BLOCK_START_KEYWORD = {
    'class', 'def', 'for', 'while',
    'if', 'elif', 'else',
    'match', 'case',
    'try', 'except', 'finally',
    'with',
}

BLOCK_SECOND_KEYWORD = {
    'elif', 'else', 'case',
    'except', 'finally',
}

BLOCK_END_KEYWORD = {
    'pass',
    'continue', 'break',
    'return', 'yield',
    'raise',
}

LOOP_KEYWORD = {'for', 'while'}

REDUCE_KEYWORD = {
    'continue': LOOP_KEYWORD,
    'return': {'def'},
}


class TokenType(Enum):
    EOL = 1
    SPACE = 2
    COMMENT = 3
    WORD = 4
    COLON = 5
    OPDEL = 6
    STRING = 7


class Token:
    def __init__(self, token_type, data):
        self.type = token_type
        self.data = data

    def __repr__(self):
        return f'({repr(self.type)}: {repr(self.data)})'

    def __str__(self):
        return self.data

    def __len__(self):
        return len(self.data)

    def __eq__(self, rhs):
        if isinstance(rhs, TokenType):
            return rhs == self.type
        if isinstance(rhs, str):
            return rhs == self.data
        if isinstance(rhs, Token):
            return (rhs.type == self.type and rhs.data == self.data)
        return False

    def __ne__(self, rhs):
        return not self.__eq__(rhs)


class BlockStatus:
    def __init__(self, line, keyword, sindent, bindent):
        self.line = line
        self.keyword = keyword
        self.sindent = sindent
        self.bindent = bindent
        self.statement = 0

    def __repr__(self):
        return (f'(BlockStatus: line={self.line},'
                f' keyword={repr(self.keyword)},'
                f' sindent={self.sindent},'
                f' bindent={self.bindent},'
                f' statement={self.statement})')


class LineStatus:
    TOKEN_EOL = Token(TokenType.EOL, '\n')

    def __init__(self, tokens):
        self.number = 0
        self.previous = 0
        self.indent = 0
        self.empty = True
        self.comment = False
        self.word = None
        self.colon = False
        self.statement = 0
        self.block_start_line = 0
        self.block_start_keyword = False
        self.block_second_keyword = False
        self.block_end_keyword = False
        self.block_enter = False
        self.block_leave = False
        self.block_stack = []
        self.token = tokens

        if not tokens:
            return

        token = tokens[0]
        if token == TokenType.SPACE:
            self.indent = self.getcolumn(token.data)
            token = tokens[1]
        if token == TokenType.WORD:
            s = token.data
            self.word = s
            self.block_start_keyword = s in BLOCK_START_KEYWORD
            self.block_second_keyword = s in BLOCK_SECOND_KEYWORD
            self.block_end_keyword = s in BLOCK_END_KEYWORD
        for p in range(len(tokens) - 1, -1, -1):
            token = tokens[p]
            if token.type not in (TokenType.EOL, TokenType.SPACE, TokenType.COMMENT):
                self.empty = False
                self.colon = (token == TokenType.COLON)
                break

        if self.empty:
            self.comment = bool(sum(token.type == TokenType.COMMENT for token in tokens))

    def __repr__(self):
        return (
            f'(LineStatus:'
            f' number={self.number},'
            f' previous={self.previous},'
            f' indent={self.indent},'
            f' empty={self.empty},'
            f' comment={self.comment},'
            f' word={self.word},'
            f' colon={self.colon},'
            f' statement={self.statement}'
            f' block_start_line={self.block_start_line},'
            f' block_start_keyword={self.block_start_keyword},'
            f' block_end_keyword={self.block_end_keyword},'
            f' block_enter={self.block_enter},'
            f' block_leave={self.block_leave},'
            f' block_stack={repr(self.block_stack)},'
            f' token={repr(self.token)})'
        )

    @staticmethod
    def getcolumn(s):
        tab = TAB_WIDTH
        n = 0
        for c in s:
            if c == ' ':
                n += 1
                continue
            if c == '\t':
                n += tab - (n % tab)
                continue
        return n

    def getline(self):
        return ''.join(t.data for t in self.token)

    def setempty(self):
        self.indent = 0
        self.empty = True
        self.token = []

    def fixeol(self):
        if self.token[-1] != TokenType.EOL:
            self.token += (self.TOKEN_EOL,)


class Lexer:
    WCHARS = {c for c in (
        '0123456789'
        'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
        'abcdefghijklmnopqrstuvwxyz'
        '_'
    )}

    OPDELS = {
        '+', '-', '*', '**', '/', '//', '%', '@',
        '<<', '>>', '&', '|', '^', '~', ':=',
        '<', '>', '<=', '>=', '==', '!=',

        '+=', '-=', '*=', '**=', '/=', '//=', '%=', '@=',
        '<<=', '>>=', '&=', '|=', '^=',

        '->', '...',

        '(', ')', '[', ']', '{', '}',
    }

    def __init__(self, stream, **option):
        self.S = Lexer
        self.WCHARS = self.S.WCHARS
        self.OPDELS = self.S.OPDELS
        self.stream = stream
        self.debug = option['debug'] if 'debug' in option else False

    def gettoken(self):
        ch = self.stream.getchar()
        if ch is None:
            return None
        if ch == '\n':
            return Token(TokenType.EOL, ch)
        if ord(ch) < 33:
            return self.getspace(ch)
        if ch in self.WCHARS or ord(ch) >= 0x80:
            return self.getword(ch)
        if ch == '"':
            s, t = self.stream.read(2)
            if s == '""':
                return self.getlongquote(ch + s)
            self.stream.write(t)
            return self.getquote(ch)
        if ch == "'":
            return self.getquote(ch)
        if ch == '#':
            return self.getcomment(ch)

        s, t = self.stream.read(2)
        if len(s) > 1:
            u = ch + s
            if u in self.OPDELS:
                return Token(TokenType.OPDEL, u)
            self.stream.putchar(t[1])
        if s:
            u = ch + s[0]
            if u in self.OPDELS:
                return Token(TokenType.OPDEL, u)
            if u == '\\\n':
                return self.getspace(u)
            self.stream.putchar(t[0])

        if ch == ':':
            return Token(TokenType.COLON, ch)
        return Token(TokenType.OPDEL, ch)

    def getspace(self, space):
        stream = self.stream
        while True:
            ch = stream.getchar()
            if ch is None:
                return Token(TokenType.SPACE, space)
            if ch == '\n':
                break
            if ord(ch) < 33:
                space += ch
                continue
            if ch != '\\':
                break
            space += ch
            c2 = stream.getchar()
            if ord(c2) < 33:
                space += c2
                continue
            stream.putchar(c2)
            break
        stream.putchar(ch)
        return Token(TokenType.SPACE, space)

    def getword(self, word):
        WCHARS = self.WCHARS
        stream = self.stream
        while True:
            ch = stream.getchar()
            if ch is None:
                break
            if ch not in WCHARS and ord(ch) < 0x80:
                stream.putchar(ch)
                break
            word += ch
        return Token(TokenType.WORD, word)

    def getquote(self, qstr):
        stream = self.stream
        escape = False
        qch = str(qstr)
        while True:
            ch = stream.getchar()
            if ch is None:
                break
            if escape:
                qstr += ch
                escape = False
                continue
            if ch == '\n':
                stream.putchar(ch)
                break
            qstr += ch
            if ch == '\\':
                escape = True
                continue
            if ch == qch:
                break
        return Token(TokenType.STRING, qstr)

    def getlongquote(self, qstr):
        stream = self.stream
        while True:
            ch = stream.getchar()
            if ch is None:
                break
            if ch == '"':
                s, t = stream.read(2)
                if s == '""':
                    qstr += '"""'
                    break
                stream.write(t)
            qstr += ch
        return Token(TokenType.STRING, qstr)

    def getcomment(self, cstr):
        stream = self.stream
        while True:
            ch = stream.getchar()
            if ch is None:
                break
            if ch == '\n':
                stream.putchar(ch)
                break
            cstr += ch
        return Token(TokenType.COMMENT, cstr)


class Parser(Lexer):
    BRACKET_OPEN = {'(': ')', '[': ']', '{': '}'}
    BRACKET_CLOSE = {')', ']', '}'}

    def __init__(self, stream, **option):
        super().__init__(stream, **option)

        self.S = Parser
        self.BRACKET_OPEN = self.S.BRACKET_OPEN
        self.BRACKET_CLOSE = self.S.BRACKET_CLOSE
        self.DEF_BLOCK_END = option['defend'] if 'defend' in option else 'pass'
        self.LOOP_BLOCK_END = option['loopend'] if 'loopend' in option else 'pass'
        self.classdefnl = option['classdefnl'] if 'classdefnl' in option else False
        self.error = []

        lines = []

        blnum = 0
        bkeyword = '[main]'
        bstat = BlockStatus(blnum, bkeyword, -1, 0)
        bstack = [bstat]

        plnum = 0
        pindent = 0
        lindent = 0
        benter = None
        slnum = None
        while True:
            if slnum:
                plnum = slnum
                slnum = None

            line = self.readline()
            if not line:
                break

            lnum = len(lines)
            if not line.empty:
                for st in bstack:
                    st.statement += 1

                lindent = line.indent
                line.block_enter = lindent > pindent
                line.block_leave = lindent < pindent

                if line.block_leave:
                    while lindent <= bstack[-1].sindent:
                        bstack.pop()
                        bstat = bstack[-1]
                    blnum = bstat.line
                    bkeyword = bstat.keyword

                if benter:
                    bstat.bindent = lindent
                    benter = False

                if line.block_start_keyword and line.colon:
                    if lindent <= bstat.sindent:
                        bstack.pop()
                    blnum = lnum
                    bkeyword = line.word
                    bstat = BlockStatus(blnum, bkeyword, lindent, lindent)
                    bstack.append(bstat)
                    benter = True

                slnum = lnum
                pindent = lindent

            elif line.comment and lindent == line.indent:
                slnum = lnum

            line.number = lnum
            line.previous = plnum
            line.block_start_line = blnum
            line.block_stack = list(bstack)
            line.statement = bstat.statement

            lines.append(line)

        self.lines = lines
        self.last_statement = plnum

    def readline(self):
        line = []
        stack = []
        while True:
            token = self.gettoken()
            if token is None:
                break
            line.append(token)
            if token == TokenType.EOL:
                if stack:
                    continue
                break
            s = token.data
            if s in self.BRACKET_OPEN:
                stack.append(self.BRACKET_OPEN[s])
                continue
            if s in self.BRACKET_CLOSE:
                if stack and s != stack.pop():
                    self.error.append((s, "error: unmatched parentheses/bracket"))
                    stack = []
                continue
        if not line:
            return None

        return LineStatus(tuple(line))

    def append_block_end(self):
        if self.lines:
            self.lines[-1].fixeol()
        lnum = self.last_statement
        if lnum > 1:
            self.insert_block_end(lnum, 0, False)
        while lnum > 1:
            line = self.lines[lnum]
            if not line.block_leave:
                lnum -= 1
                continue
            lnum = line.previous
            self.insert_block_end(lnum, line.indent, line.block_second_keyword)

    def insert_block_end(self, lnum, indent, second):
        line = self.lines[lnum]
        bstack = line.block_stack
        spos = reduce(lambda v, s: v + int(s.bindent <= indent), bstack, 0)
        epos = len(bstack)

        iline = line
        ispace = ''
        iekw = False
        if iline.token:
            token = iline.token[0]
            if token == TokenType.SPACE:
                ispace = token.data
                iekw = (len(iline.token) >= 2 and
                        iline.token[1].data in BLOCK_END_KEYWORD)

        eol = line.TOKEN_EOL
        for stat in reversed(bstack[spos:epos]):
            stack = bstack[:epos]
            epos -= 1

            space = ' ' * stat.bindent
            if space == ispace and iekw:
                continue

            bkw = stat.keyword
            ekw = 'pass'
            if bkw == 'def':
                ekw = self.DEF_BLOCK_END
            elif bkw in LOOP_KEYWORD:
                ekw = self.LOOP_BLOCK_END
            if (self.classdefnl and bkw == 'class' and
                    line.block_stack[-1].keyword == 'def'):
                lnum += 1
                line = LineStatus([eol])
                line.block_stack = stack
                self.lines.insert(lnum, line)
            lnum += 1
            token = [Token(TokenType.SPACE, space),
                     Token(TokenType.WORD, ekw), eol]
            line = LineStatus(token)
            line.block_stack = stack
            self.lines.insert(lnum, line)

    def remove_block_end(self):
        for line in self.lines:
            if not line.block_leave:
                continue
            lnum = line.previous
            while lnum > 1:
                line = self.lines[lnum]
                if line.statement < 2:
                    break
                skw = line.block_stack[-1].keyword
                ekw = line.getline().strip()
                if skw not in REDUCE_KEYWORD.get(ekw, tuple()):
                    break
                line.setempty()
                lnum -= 1

        for line in self.lines:
            if line.statement >= 2 and line.getline().strip() == 'pass':
                line.setempty()
                if line.block_stack[-1].keyword != 'class':
                    continue
                lnum = line.number
                while lnum > 2:
                    lnum -= 1
                    line = self.lines[lnum]
                    if line.getline().strip():
                        break
                    line.setempty()

        for line in reversed(self.lines):
            if line.empty:
                continue
            if line.statement < 2:
                break
            skw = line.block_stack[-1].keyword
            ekw = line.getline().strip()
            if skw in REDUCE_KEYWORD.get(ekw, tuple()):
                line.setempty()
            break


    def getsource(self):
        r = ''
        for line in self.lines:
            r += line.getline()
        return r


# -----------------------------------------------------------------------------


if __name__ == '__main__':
    def main():
        import argparse
        global TAB_WIDTH

        parser = argparse.ArgumentParser()
        parser.add_argument('-d', '--debug', action='store_true', default=False)
        parser.add_argument('-q', '--quiet', action='store_true', default=False)
        parser.add_argument('-e', '--error-stop', action='store_true', default=False)
        parser.add_argument('-a', '--append', action='store_true', default=False)
        parser.add_argument('-r', '--remove', action='store_true', default=False)
        parser.add_argument('--tab-width', type=int, metavar='N', default=DEFAULT_TAB_WIDTH)
        parser.add_argument('--class-def-nl', action='store_false', default=True)
        parser.add_argument('--def-return', action='store_true', default=False)
        parser.add_argument('--loop-continue', action='store_true', default=False)
        parser.add_argument('inpfile', metavar='INP', nargs='?', default='-')
        parser.add_argument('outfile', metavar='OUT', nargs='?', default='-')

        args = parser.parse_args()
        TAB_WIDTH = args.tab_width
        classdefnl = args.class_def_nl
        defend = 'return' if args.def_return else 'pass'
        loopend = 'continue' if args.loop_continue else 'pass'

        if args.debug:
            print(f'option: tab = {TAB_WIDTH}')
            print(f'option: def - {defend}')
            print(f'option: loop - {loopend}')
        else:
            if not args.append and not args.remove:
                print('option error: set either option {-a|--append} or {-r|--remove}.')
                parser.print_help()
                return
            if args.append and args.remove:
                print('option error: conflicting options --append and --remove.')
                parser.print_help()
                return

        parser = Parser(ReadStream(args.inpfile),
                        classdefnl=classdefnl,
                        defend=defend,
                        loopend=loopend,
                        debug=args.debug)
        if parser.error and args.error_stop:
            for error, msg in parser.error:
                sys.stderr.write(f'{error.name}:{error.lnum}: {msg}\n')
            if args.error_stop:
                sys.exit(2)

        if args.remove:
            parser.remove_block_end()
        if args.append:
            parser.append_block_end()
        if args.debug and args.outfile == '.':
            return
        WriteStream(args.outfile).write(parser.getsource())

    main()

# -----------------------------------------------------------------------------
