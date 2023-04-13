#!/usr/bin/env python3

import sys
from enum import Enum

# -----------------------------------------------------------------------------

DEFAULT_TAB_WIDTH = 4

KEYWORD_START = {
    'class', 'def', 'for', 'while',
    'if', 'elif', 'else',
    'match', 'case',
    'try', 'except', 'finally',
    'with',
}

KEYWORD_SECOND = {
    'elif', 'else', 'case',
    'except', 'finally',
}

KEYWORD_BLOCK = KEYWORD_START | KEYWORD_SECOND

KEYWORD_END = {
    'pass',
    'continue', 'break',
    'return', 'yield',
    'raise',
}

KEYWORD_LOOP = {'for', 'while'}

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
        return ('(String'
                f': name={repr(self.name)}'
                f', line={self.line}'
                f', offset={self.offset}'
                f', str={repr(super())}'
                ')')

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
        self.stdin = name == '-'
        self.bstream = bool(stream)
        self.name = '[STDIN]' if self.stdin else name
        self.stream = (stream if stream else
                       sys.stdin if self.stdin else open(name))
        self.lines = []
        self.line = ''
        self.number = 0
        self.offset = 0
        self.pending = []

    def __enter__(self):
        if not self.stdin and not self.bstream:
            self.stream.__enter__()
        return self

    def __exit__(self, etype, value, trace):
        if not self.stdin and not self.bstream:
            return self.stream.__exit__(etype, value, trace)
        return None

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

    def readchar(self, count):
        chars = tuple(self.getchar() for _ in range(count))
        return (''.join(filter(lambda v: v, chars)), chars)

    def writechar(self, chars):
        self.pending += reversed(chars)
        return self

    def emptychar(self):
        return String.create(self.name, self.number, self.offset, '')

    def read(self):
        return self.stream.read()


class WriteStream:
    def __init__(self, name, stream=None):
        self.stdout = name == '-'
        self.bstream = bool(stream)
        self.name = '[STDOUT]' if self.stdout else name
        self.stream = (stream if stream else
                       (sys.stdout if self.stdout else open(name, 'w')))

    def __enter__(self):
        if not self.stdout and not self.bstream:
            self.stream.__enter__()
        return self

    def __exit__(self, etype, value, trace):
        if not self.stdout and not self.bstream:
            return self.stream.__exit__(etype, value, trace)
        return None

    def write(self, data):
        return self.stream.write(data)


# -----------------------------------------------------------------------------


class CharType(Enum):
    EOL = 1
    SPACE = 2
    WORD = 3
    QUOTE = 4
    DQUOTE = 5
    COMMENT = 6
    OPDEL = 7


class TokenType(Enum):
    EOL = 1
    SPACE = 2
    COMMENT = 3
    WORD = 4
    COLON = 5
    OPDEL = 6
    STRING = 7
    ERROR = -1


class Token:
    def __init__(self, token_type, data=''):
        self.type = token_type
        self.data = data

    def __repr__(self):
        return ('(Token'
                f': type={repr(self.type)}'
                f', data={repr(self.data)}'
                ')')

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


class LineStatus:
    def __init__(self, tokens, source_line):
        self.block = None
        self.number = None
        self.indent = 0
        self.error = False
        self.empty = True
        self.comment = False
        self.word = None
        self.keyword_start = None
        self.keyword_second = None
        self.keyword_block = None
        self.keyword_end = None
        self.statement = 0
        self.source_line = source_line
        self.token = tokens

    def __repr__(self):
        return ('(LineStatus'
                f': number={self.number}'
                f', indent={self.indent}'
                f', error={self.error}'
                f', empty={self.empty}'
                f', comment={self.comment}'
                f', keyword_block={repr(self.keyword_block)}'
                f', keyword_start={repr(self.keyword_start)}'
                f', keyword_second={repr(self.keyword_second)}'
                f', keyword_end={repr(self.keyword_end)}'
                f', statement={repr(self.statement)}'
                f', source_line={repr(self.source_line)}'
                f', token={repr(self.token)}'
                ')')

    def iscomment(self):
        return self.empty and self.comment

    def getline(self):
        return ''.join(str(t) for t in self.token)

    @staticmethod
    def inkeyword(table, word):
        return word if word in table else None

    def setword(self, word):
        self.word = word
        self.keyword_start = self.inkeyword(KEYWORD_START, word)
        self.keyword_second = self.inkeyword(KEYWORD_SECOND, word)
        self.keyword_end = self.inkeyword(KEYWORD_END, word)

    def setempty(self):
        self.indent = 0
        self.empty = True
        self.comment = False
        self.token = []

    def fixeol(self):
        if self.token[-1] != TokenType.EOL:
            self.token += (TOKEN_EOL,)

    def getdebug(self):
        ltext = self.getline()
        if ltext and ltext[-1] == '\n':
            ltext = ltext[:-1]
        return (f'{"E" if self.error else "N"}={self.number:03}:{self.source_line:03}:'
                f'L={self.block.level if self.block else 0}:'
                f'I={self.indent:02}:'
                f'S={self.statement:02}:'
                f'{self.block.keyword if self.block else "<<NONE>>":8}:'
                f'{ltext}')


class BlockStatus:
    def __init__(self, parent=None, keyword=None, line_start=None, indent_start=0):
        self.parent = parent
        self.children = []
        self.keyword = keyword if keyword else '[[main]]'
        self.level = parent.level + 1 if parent else 0
        self.indent_start = indent_start
        self.indent_block = 0
        self.line_start = line_start
        self.line_end = None
        self.statement = 0

    def __repr__(self):
        return ('(BlockStatus'
                f': keyword={repr(self.keyword)}'
                f', level={self.level}'
                f', indent_start={self.indent_start}'
                f', indent_block={self.indent_block}'
                f', line_start={self.line_start}'
                f', line_end={self.line_end}'
                f', statement={self.statement}'
                ')')

    def append(self, child):
        self.children.append(child)


class Lexer:
    BRACKET_OPEN = {'(': ')', '[': ']', '{': '}'}
    BRACKET_CLOSE = {')', ']', '}'}

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

        '!', '$', ',', '.', ';', '?', '\\', '`',
    }

    def __init__(self, stream, **option):
        self.char_type = {
            '\n': self.geteol,
            "'": self.getquote,
            '"': self.getdquote,
            '#': self.getcomment,
            '\x7F': self.getword,
        }
        self.char_type.update({chr(c): self.getspace for c in range(33) if c != 10})
        self.char_type.update({c: self.getword for c in Lexer.WORD_CHAR})
        self.char_type.update({c[0]: self.getopdel for c in Lexer.OPDELS})

        self.debug = self.getoption(option, 'debug', False)
        self.tab_width = self.getoption(option, 'tab', DEFAULT_TAB_WIDTH)
        self.stream = stream
        self.errors = []

    def seterror(self, err):
        self.errors.append(err)
        return self

    @staticmethod
    def getoption(option, name, defval):
        return option[name] if name in option else defval

    def getcolumn(self, line):
        tab = self.tab_width
        column = 0
        for char in line:
            if char == ' ':
                column += 1
                continue
            if char == '\t':
                column += tab - (column % tab)
                continue
        return column

    def gettoken(self):
        char = self.stream.getchar()
        return self.char_type[min(char, '\x7F')](char) if char else None

    def geteol(self, char):
        return Token(TokenType.EOL, char)

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
        for char in iter(stream.getchar, None):
            if char not in word_char and ord(char) < 0x80:
                stream.putchar(char)
                break
            word += char
        return Token(TokenType.WORD, word)

    def getquote(self, qstr):
        stream = self.stream
        escape = False
        qch = str(qstr)
        for char in iter(stream.getchar, None):
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

    def getdquote(self, char):
        next_str, next_chars = self.stream.readchar(2)
        if next_str == '""':
            return self.getlongquote(char + next_str)
        self.stream.writechar(next_chars)
        return self.getquote(char)

    def getlongquote(self, qstr):
        stream = self.stream
        for char in iter(stream.getchar, None):
            if char == '"':
                next_str, next_chars = stream.readchar(2)
                if next_str == '""':
                    qstr += '"""'
                    break
                stream.writechar(next_chars)
            qstr += char
        return Token(TokenType.STRING, qstr)

    def getcomment(self, cstr):
        stream = self.stream
        for char in iter(stream.getchar, None):
            if char == '\n':
                stream.putchar(char)
                break
            cstr += char
        return Token(TokenType.COMMENT, cstr)

    def getopdel(self, char):
        next_str, next_chars = self.stream.readchar(2)
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

    def getline(self):
        error = False
        tokens = []
        stack = []
        for token in iter(self.gettoken, None):
            tokens.append(token)
            if token == TokenType.EOL:
                if stack:
                    continue
                break
            char = token.data
            if char in self.BRACKET_OPEN:
                stack.append(self.BRACKET_OPEN[char])
                continue
            if char in self.BRACKET_CLOSE:
                if not stack or char != stack.pop():
                    self.seterror((char, "error: unmatched parentheses/bracket"))
                    error = True
                continue
        if not tokens:
            return None
        if error:
            tokens.insert(0, Token(TokenType.ERROR))
        return tokens


class Parser(Lexer):
    def __init__(self, stream, **option):
        super().__init__(stream, **option)
        self.debug_line = self.getoption(option, 'debug_line', False)
        self.debug_append = self.getoption(option, 'debug_append', False)
        self.debug_remove = self.getoption(option, 'debug_remove', False)

        self.def_block_end = self.getoption(option, 'defend', 'pass')
        self.loop_block_end = self.getoption(option, 'loopend', 'pass')
        self.classdefnl = self.getoption(option, 'classdefnl', False)

        self.append_keyword_map = {
            'def': self.def_block_end,
            'for': self.loop_block_end,
            'while': self.loop_block_end,
        }
        self.remove_keyword_map = {
            None: {},
            'pass': KEYWORD_BLOCK,
            'continue': KEYWORD_LOOP, 'break': {},
            'return': 'def', 'yield': {},
            'raise': {},
        }

        self.error = False
        self.block = BlockStatus(None, '[[TEXT]]', 0)
        self.block_stack = [self.block]
        self.block_enter = None
        self.last_indent = 0
        self.line = []
        self.indent_table = {}
        self.indent_list = []
        self.indent_depth = self.tab_width

        self.append_newline = False

        self.block.statement = 1
        self.parse_line()

        if self.debug_line:
            for line in self.line:
                print(line.getdebug())

    def getsource(self):
        return ''.join(line.getline() for line in self.line)

    def push_block(self, block):
        self.block.append(block)
        self.block_stack.append(block)
        self.block = block
        return block

    def pop_block(self):
        block = self.block_stack.pop()
        self.block = self.block_stack[-1]
        self.block.statement += block.statement
        return block

    def add_indent(self, indent, space):
        indent_map = self.indent_table.get(indent, None)
        if not indent_map:
            indent_map = {}
            self.indent_table[indent] = indent_map
        indent_count = indent_map.get(space, 0)
        indent_map[space] = indent_count + 1

    def nextline(self, tokens):
        token = tokens[0]
        status = LineStatus(tokens, token.data.line)
        if token == TokenType.SPACE:
            status.indent = self.getcolumn(token.data)
            self.add_indent(status.indent, token.data)
            token = None if len(tokens) < 2 else tokens[1]
        elif token == TokenType.ERROR:
            status.error = True
            return status
        if token:
            if token == TokenType.COMMENT:
                status.comment = True
            elif token != TokenType.EOL:
                status.empty = False
                if token == TokenType.WORD:
                    status.setword(token.data)
        for token in reversed(status.token):
            if token.type not in (TokenType.EOL, TokenType.SPACE, TokenType.COMMENT):
                if token == TokenType.COLON:
                    status.keyword_block = status.keyword_start or status.keyword_second
                break
        return status

    def set_block_enter(self, status):
        self.block.indent_block = self.last_indent
        self.block_enter = BlockStatus(
            self.block, status.keyword_block, status.number, status.indent)

    def set_block_leave(self, line_number):
        if len(self.block_stack) > 1:
            self.block.line_end = line_number
            self.pop_block()
            self.last_indent = self.block.indent_block
            return True
        return False

    def parse_line(self):
        for tokens in iter(self.getline, None):
            status = self.nextline(tokens)
            status.number = len(self.line)
            self.line.append(status)

            if self.block_enter:
                self.push_block(self.block_enter)
                self.block_enter = None
            status.block = self.block

            if status.empty:
                continue

            if status.indent == self.last_indent:
                if not self.block.statement and status.keyword_block:
                    self.set_block_leave(status.number)
                self.parse_block_statement(status)
            elif status.indent > self.last_indent:
                self.parse_block_enter(status)
            else:
                self.parse_block_leave(status)

        if self.block_enter:
            self.push_block(self.block_enter)

        line_number = len(self.line)
        while self.set_block_leave(line_number):
            pass
        self.block.line_end = line_number

    def parse_block_statement(self, status):
        self.block.statement += 1
        status.statement = self.block.statement
        if self.error or status.error:
            self.error = True
            return False
        if status.keyword_block:
            self.set_block_enter(status)
        return True

    def parse_block_enter(self, status):
        if self.block.statement:
            self.error = True
            return False
        if not self.parse_block_statement(status):
            return False
        self.last_indent = status.indent
        self.block.indent_block = status.indent
        return True

    def parse_block_leave(self, status):
        if status.indent > self.block.indent_start:
            self.last_indent = status.indent
            self.error = True
            return False

        self.error = False
        while (self.set_block_leave(status.number) and
               status.indent <= self.block.indent_start):
            pass
        status.block = self.block
        self.last_indent = status.indent
        return self.parse_block_statement(status)

    def fixeof(self):
        if self.line:
            self.line[-1].fixeol()

    def scan_block_end(self, indent, start, end):
        line_number = end - 1
        while line_number > start:
            line = self.line[line_number]
            if not line.empty or line.comment:
                if indent <= line.indent:
                    return line_number
            line_number -= 1
        return line_number

    def initialize_indent_table(self):
        def most(iterable):
            return sorted(iterable, reverse=True)[0][1]
        for key in self.indent_table:
            self.indent_table[key] = most(
                (item[1], item[0]) for item in self.indent_table[key].items())
        indents = sorted(self.indent_table)
        self.indent_list = indents
        delta = [indents[n+1] - indents[n] for n in range(len(indents) - 1)]
        if delta:
            self.indent_depth = most((delta.count(n), n) for n in set(delta))

    def get_indent_space(self, indent):
        space = self.indent_table.get(indent, None)
        if space:
            return space
        for pos in self.indent_list:
            if indent < pos:
                return self.indent_table[pos]
        return ' ' * indent

    def append_block_end(self):
        self.initialize_indent_table()
        self.insert_children_block_end(self.block)

    def insert_children_block_end(self, block):
        for child in reversed(block.children):
            self.insert_block_end(child)

    def insert_block_end(self, block):
        keyword = self.append_keyword_map.get(block.keyword, 'pass')
        line_number = self.scan_block_end(block.indent_block,
                                          block.line_start, block.line_end)

        if self.debug_append:
            print('BLOCK'
                  f':{block.line_start:03}-{block.line_end:03}'
                  f':L={block.level}'
                  f':S={block.statement}'
                  f':{self.line[block.line_start].keyword_block} -> {keyword}')

        line = self.line[line_number]
        if block.indent_block < line.indent or not line.keyword_end:
            indent = block.indent_block
            if line_number == block.line_start:
                indent += self.indent_depth
            line_number += 1
            token = [Token(TokenType.SPACE, self.get_indent_space(indent)),
                     Token(TokenType.WORD, keyword),
                     TOKEN_EOL]
            status = LineStatus(token, line.source_line)
            status.number = line_number
            self.line.insert(status.number, status)

        if self.classdefnl:
            newline = self.append_newline
            self.append_newline = False
            if block.keyword == 'class':
                self.append_newline = True
            elif (newline and block.keyword == 'def' and
                  self.line[line_number + 1].getline().strip()):
                line_number += 1
                status = LineStatus([TOKEN_EOL], line.source_line)
                status.number = line_number
                self.line.insert(status.number, status)

        self.insert_children_block_end(block)

    def remove_block_end(self):
        self.delete_children_block_end(self.block)

    def delete_children_block_end(self, block):
        for child in reversed(block.children):
            self.delete_block_end(child)

    def delete_block_end(self, block):
        if self.debug_remove:
            print('BLOCK'
                  f':{block.line_start:03}-{block.line_end:03}'
                  f':L={block.level}'
                  f':S={block.statement}'
                  f':{self.line[block.line_start].keyword_block}')

        remove_empty = False
        line_end = block.line_end
        while line_end > block.line_start:
            line_number = self.scan_block_end(block.indent_block,
                                              block.line_start, line_end)
            if remove_empty:
                for lnum in range(line_number + 1, line_end):
                    self.line[lnum].setempty()

            line = self.line[line_number]
            if line.comment or not line.empty:
                if (line.indent <= block.indent_start or
                    block.indent_block < line.indent or
                    line.comment or line.statement < 2 or
                    not line.keyword_end):
                    break
                if (block.keyword not in self.remove_keyword_map[line.keyword_end] or
                    line.keyword_end != line.getline().strip()):
                    break
            line.setempty()
            remove_empty = True
            line_end = line_number

        self.delete_children_block_end(block)


# -----------------------------------------------------------------------------


if __name__ == '__main__':
    import argparse

    def main():
        parser = argparse.ArgumentParser()
        parser.add_argument('-q', '--quiet', action='store_true', default=False)
        parser.add_argument('-e', '--error-stop', action='store_true', default=False)
        parser.add_argument('-a', '--append', action='store_true', default=False)
        parser.add_argument('-r', '--remove', action='store_true', default=False)
        parser.add_argument('--tab-width', type=int, metavar='N', default=DEFAULT_TAB_WIDTH)
        parser.add_argument('--class-def-nl', action='store_false', default=True)
        parser.add_argument('--def-return', action='store_true', default=False)
        parser.add_argument('--loop-continue', action='store_true', default=False)

        parser.add_argument('-d', '--debug', action='store_true', default=False)
        parser.add_argument('--debug-line', action='store_true', default=False)
        parser.add_argument('--debug-append', action='store_true', default=False)
        parser.add_argument('--debug-remove', action='store_true', default=False)

        parser.add_argument('inpfile', metavar='INP', nargs='?', default='-')
        parser.add_argument('outfile', metavar='OUT', nargs='?', default='-')

        args = parser.parse_args()
        tab_width = args.tab_width
        classdefnl = args.class_def_nl
        defend = 'return' if args.def_return else 'pass'
        loopend = 'continue' if args.loop_continue else 'pass'

        if not args.debug:
            if not args.append and not args.remove:
                print('option error: set either option {-a|--append} or {-r|--remove}.')
                parser.print_help()
                return
            if args.append and args.remove:
                print('option error: conflicting options --append and --remove.')
                parser.print_help()
                return

        with ReadStream(args.inpfile) as stream:
            parser = Parser(stream,
                            tab_width=tab_width,
                            classdefnl=classdefnl,
                            defend=defend,
                            loopend=loopend,
                            debug_line=args.debug_line or args.debug,
                            debug_append=args.debug_append or args.debug,
                            debug_remove=args.debug_remove or args.debug,
                            debug=args.debug)

        if parser.error and args.error_stop:
            for error, msg in parser.errors:
                sys.stderr.write(f'{error.name}:{error.line}: {msg}\n')
            if args.error_stop:
                sys.exit(2)

        if args.append or args.remove:
            parser.fixeof()
        if args.remove:
            parser.remove_block_end()
        if args.append:
            parser.append_block_end()
        if args.debug and args.outfile == '.':
            return

        with WriteStream(args.outfile) as stream:
            stream.write(parser.getsource())

    main()


# -----------------------------------------------------------------------------
