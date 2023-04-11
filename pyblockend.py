#!/usr/bin/env python3

import sys

from enum import Enum
from functools import reduce

# -----------------------------------------------------------------------------

DEFAULT_TAB_WIDTH = 4
TAB_WIDTH = DEFAULT_TAB_WIDTH

# -----------------------------------------------------------------------------


class String(str):
    def __init__(self, *args):
        if args:
            arg0 = args[0]
            if isinstance(arg0, String):
                self.name = arg0.name
                self.line = arg0.line
                self.offset = arg0.offset
                return
        self.name = None
        self.line = None
        self.offset = None

    def __repr__(self):
        return ('(String:'
                f' name={repr(self.name)},'
                f' line={self.line},'
                f' offset={self.offset},'
                f' str={repr(super())})')

    def __add__(self, rhs):
        return String.create(self.name, self.line, self.offset, super().__add__(rhs))

    def setparam(self, name, line, offset):
        self.name = name
        self.line = line
        self.offset = offset
        return self

    @staticmethod
    def create(name, line, offset, data=''):
        return String(data).setparam(name, line, offset)


class ReadStream:
    def __init__(self, name, stream=None):
        self.name = name if name != '-' else '[STDIN]'
        self.stream = (stream if stream else
                       (sys.stdin if name == '-' else open(name)))
        self.lines = []
        self.line = ''
        self.number = 0
        self.offset = 0
        self.pending = []

    def getline(self):
        line = self.stream.readline()
        if line:
            self.lines.append(line)
            self.line = line
            self.number += 1
            self.offset = 0
        return line

    def getchar(self):
        if self.pending:
            return self.pending.pop()
        if self.offset >= len(self.line):
            if not self.getline():
                return None
        offset = self.offset
        self.offset = offset + 1
        return String.create(self.name, self.number, offset, self.line[offset])

    def putchar(self, char):
        self.pending.append(char)
        return self

    def read(self, count):
        chars = tuple(self.getchar() for _ in range(count))
        return (''.join(c or '' for c in chars), chars)

    def write(self, chars):
        self.pending += reversed(chars)
        return self

    def emptychar(self):
        return String.create(self.name, self.number, self.offset, '')


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
        return ('(Token:'
                f' type={repr(self.type)},'
                f' data={repr(self.data)})')

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


TOKEN_EOL = Token(TokenType.EOL, '\n')


class BlockStatus:
    def __init__(self, keyword, line, indent_start, indent_block):
        self.keyword = keyword
        self.line = line
        self.indent_start = indent_start
        self.indent_block = indent_block
        self.statement = 0

    def __repr__(self):
        return ('(BlockStatus:'
                f' keyword={repr(self.keyword)},'
                f' line={self.line},'
                f' indent_start={self.indent_start},'
                f' indent_block={self.indent_block},'
                f' statement={self.statement})')


class LineStatus:
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
            word = token.data
            self.word = word
            self.block_start_keyword = word in BLOCK_START_KEYWORD
            self.block_second_keyword = word in BLOCK_SECOND_KEYWORD
            self.block_end_keyword = word in BLOCK_END_KEYWORD
        for index in range(len(tokens) - 1, -1, -1):
            token = tokens[index]
            if token.type not in (TokenType.EOL, TokenType.SPACE, TokenType.COMMENT):
                self.empty = False
                self.colon = token == TokenType.COLON
                break

        if self.empty:
            self.comment = bool(sum(token.type == TokenType.COMMENT for token in tokens))

    def __repr__(self):
        return ('(LineStatus:'
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
                f' token={repr(self.token)})')

    def isblock(self, keyword):
        return self.block_stack[-1].keyword == keyword

    def iscomment(self):
        return self.empty and self.comment

    def checkcomment(self, indent):
        return self.iscomment() and self.indent == indent

    def getdebug(self):
        ltext = self.getline().replace('\n', '')
        return (f'N={self.number:03}'
                f':P={self.previous:03}'
                f':D={len(self.block_stack)}'
                f':S={self.statement:02}'
                f':{self.block_stack[-1].keyword:8}'
                f':{ltext}')

    @staticmethod
    def getcolumn(line):
        tab = TAB_WIDTH
        column = 0
        for char in line:
            if char == ' ':
                column += 1
                continue
            if char == '\t':
                column += tab - (column % tab)
                continue
        return column

    def getline(self):
        return ''.join(t.data for t in self.token)

    def setempty(self):
        self.indent = 0
        self.empty = True
        self.token = []

    def fixeol(self):
        if self.token[-1] != TokenType.EOL:
            self.token += (TOKEN_EOL,)


class Lexer:
    WORD_CHAR = set(
        '0123456789'
        'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
        'abcdefghijklmnopqrstuvwxyz'
        '_'
    )

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
        self.stream = stream
        self.debug = option['debug'] if 'debug' in option else False

    def gettoken(self):
        char = self.stream.getchar()
        if char is None:
            return None
        if char == '\n':
            return TOKEN_EOL
        if ord(char) < 33:
            return self.getspace(char)
        if char in self.WORD_CHAR or ord(char) >= 0x80:
            return self.getword(char)
        if char == '"':
            next_str, next_chars = self.stream.read(2)
            if next_str == '""':
                return self.getlongquote(char + next_str)
            self.stream.write(next_chars)
            return self.getquote(char)
        if char == "'":
            return self.getquote(char)
        if char == '#':
            return self.getcomment(char)

        next_str, next_chars = self.stream.read(2)
        if len(next_str) > 1:
            check_str = char + next_str
            if check_str in self.OPDELS:
                return Token(TokenType.OPDEL, check_str)
            self.stream.putchar(next_chars[1])
        if next_str:
            check_str = char + next_str[0]
            if check_str in self.OPDELS:
                return Token(TokenType.OPDEL, check_str)
            if check_str == '\\\n':
                return self.getspace(check_str)
            self.stream.putchar(next_chars[0])

        if char == ':':
            return Token(TokenType.COLON, char)
        return Token(TokenType.OPDEL, char)

    def getspace(self, space):
        stream = self.stream
        while True:
            char = stream.getchar()
            if char is None:
                return Token(TokenType.SPACE, space)
            if char == '\n':
                break
            if ord(char) < 33:
                space += char
                continue
            if char != '\\':
                break
            space += char
            next_char = stream.getchar()
            if ord(next_char) < 33:
                space += next_char
                continue
            stream.putchar(next_char)
            break
        stream.putchar(char)
        return Token(TokenType.SPACE, space)

    def getword(self, word):
        word_char = self.WORD_CHAR
        stream = self.stream
        while True:
            char = stream.getchar()
            if char is None:
                break
            if char not in word_char and ord(char) < 0x80:
                stream.putchar(char)
                break
            word += char
        return Token(TokenType.WORD, word)

    def getquote(self, qstr):
        stream = self.stream
        escape = False
        qch = str(qstr)
        while True:
            char = stream.getchar()
            if char is None:
                break
            if escape:
                qstr += char
                escape = False
                continue
            if char == '\n':
                stream.putchar(char)
                break
            qstr += char
            if char == '\\':
                escape = True
                continue
            if char == qch:
                break
        return Token(TokenType.STRING, qstr)

    def getlongquote(self, qstr):
        stream = self.stream
        while True:
            char = stream.getchar()
            if char is None:
                break
            if char == '"':
                next_str, next_chars = stream.read(2)
                if next_str == '""':
                    qstr += '"""'
                    break
                stream.write(next_chars)
            qstr += char
        return Token(TokenType.STRING, qstr)

    def getcomment(self, cstr):
        stream = self.stream
        while True:
            char = stream.getchar()
            if char is None:
                break
            if char == '\n':
                stream.putchar(char)
                break
            cstr += char
        return Token(TokenType.COMMENT, cstr)


class Parser(Lexer):
    BRACKET_OPEN = {'(': ')', '[': ']', '{': '}'}
    BRACKET_CLOSE = {')', ']', '}'}

    def __init__(self, stream, **option):
        super().__init__(stream, **option)
        self.def_block_end = option['defend'] if 'defend' in option else 'pass'
        self.loop_block_end = option['loopend'] if 'loopend' in option else 'pass'
        self.classdefnl = option['classdefnl'] if 'classdefnl' in option else False
        self.error = []

        lines = []

        block_keyword = '[main]'
        block_start_line = 0
        block_status = BlockStatus(block_keyword, block_start_line, -1, 0)
        block_stack = [block_status]

        statement_line = None
        last_statement = 0
        last_indent = 0
        line_indent = 0
        block_enter = None
        while True:
            if statement_line:
                last_statement = statement_line
                statement_line = None

            line = self.readline()
            if not line:
                break

            line_number = len(lines)
            if not line.empty:
                for stat in block_stack:
                    stat.statement += 1

                line_indent = line.indent
                line.block_enter = line_indent > last_indent
                line.block_leave = line_indent < last_indent

                if line.block_leave:
                    while line_indent <= block_stack[-1].indent_start:
                        block_stack.pop()
                        block_status = block_stack[-1]
                    block_start_line = block_status.line
                    block_keyword = block_status.keyword

                if block_enter:
                    block_status.indent_block = line_indent
                    block_enter = False

                if line.block_start_keyword and line.colon:
                    if line_indent <= block_status.indent_start:
                        block_stack.pop()
                    block_start_line = line_number
                    block_keyword = line.word
                    block_status = BlockStatus(block_keyword, block_start_line,
                                               line_indent, line_indent)
                    block_stack.append(block_status)
                    block_enter = True

                statement_line = line_number
                last_indent = line_indent

            elif line.comment and line_indent == line.indent:
                statement_line = line_number

            line.number = line_number
            line.previous = last_statement
            line.block_start_line = block_start_line
            line.block_stack = list(block_stack)
            line.statement = block_status.statement

            if self.debug:
                print(line.getdebug())

            lines.append(line)

        self.lines = lines
        self.last_statement = last_statement

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
            char = token.data
            if char in self.BRACKET_OPEN:
                stack.append(self.BRACKET_OPEN[char])
                continue
            if char in self.BRACKET_CLOSE:
                if stack and char != stack.pop():
                    self.error.append((char, "error: unmatched parentheses/bracket"))
                    stack = []
                continue
        if not line:
            return None

        return LineStatus(tuple(line))

    def append_block_end(self):
        if self.lines:
            self.lines[-1].fixeol()
        line_number = self.last_statement
        if line_number > 1:
            self.insert_block_end(line_number, len(self.lines), 0)
        while line_number > 1:
            line = self.lines[line_number]
            line_number = line.previous
            self.insert_block_end(line_number, line.number, line.indent)

    def insert_block_end(self, line_number, line_number_end, indent):
        line = self.lines[line_number]
        insert_line_number = line.number
        block_stack = line.block_stack
        stack_top = reduce(lambda v, s: v + int(s.indent_block <= indent), block_stack, 0)
        stack_end = len(block_stack)

        eol = TOKEN_EOL
        for block_status in reversed(block_stack[stack_top:stack_end]):
            line_block_stack = block_stack[:stack_end]
            stack_end -= 1

            if line.block_end_keyword and line.indent == block_status.indent_block:
                continue

            line_number_max = min(line_number_end, len(self.lines) - 1)
            skip_comment = False
            while line_number < line_number_max:
                check_line = self.lines[line_number + 1]
                if not check_line.checkcomment(block_status.indent_block):
                    break
                line = check_line
                insert_line_number = line.number
                skip_comment = True
                line_number += 1

            block_keyword = block_status.keyword
            block_end = 'pass'
            if block_keyword == 'def':
                block_end = self.def_block_end
            elif block_keyword in LOOP_KEYWORD:
                block_end = self.loop_block_end
            if (not skip_comment and self.classdefnl and
                    block_keyword == 'class' and line.isblock('def')):
                line_number += 1
                line = LineStatus([eol])
                line.number = insert_line_number
                line.block_stack = line_block_stack
                self.lines.insert(line_number, line)
                line_number_end += 1

            line_number += 1
            line_indent = ' ' * block_status.indent_block
            token = [Token(TokenType.SPACE, line_indent),
                     Token(TokenType.WORD, block_end), eol]
            line = LineStatus(token)
            line.number = insert_line_number
            line.block_stack = line_block_stack
            self.lines.insert(line_number, line)
            line_number_end += 1

    def remove_block_end(self):
        for line in self.lines:
            if not line.block_leave:
                continue
            line_number = line.previous
            while line_number > 1:
                line = self.lines[line_number]
                if line.statement < 2:
                    break
                block_keyword = line.block_stack[-1].keyword
                block_end = line.getline().strip()
                if block_keyword not in REDUCE_KEYWORD.get(block_end, tuple()):
                    break
                line.setempty()
                line_number -= 1

        for line in self.lines:
            if line.statement >= 2 and line.getline().strip() == 'pass':
                line.setempty()
                if line.block_stack[-1].keyword != 'class':
                    continue
                line_number = line.number
                while line_number > 2:
                    line_number -= 1
                    line = self.lines[line_number]
                    if line.getline().strip():
                        break
                    line.setempty()

        for line in reversed(self.lines):
            if line.empty:
                continue
            if line.statement < 2:
                break
            block_keyword = line.block_stack[-1].keyword
            block_end = line.getline().strip()
            if block_keyword in REDUCE_KEYWORD.get(block_end, tuple()):
                line.setempty()
            break

    def getsource(self):
        text = ''
        for line in self.lines:
            text += line.getline()
        return text


# -----------------------------------------------------------------------------


if __name__ == '__main__':
    import argparse

    def main():
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
                sys.stderr.write(f'{error.name}:{error.line}: {msg}\n')
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
