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


class FileString:
    def __init__(self, name, line, offset, data):
        self.name = name
        self.line = line
        self.offset = offset
        self.data = data

    def __repr__(self):
        return ('(String'
                f': name={repr(self.name)}'
                f', line={self.line}'
                f', offset={self.offset}'
                f', data={repr(self.data)}'
                ')')

    def __str__(self):
        return self.data

    def __format__(self, fmt):
        return self.data.__format__(fmt)

    def __hash__(self):
        return hash(self.data)

    def __iter__(self):
        return iter(self.data)

    def __len__(self):
        return len(self.data)

    def __eq__(self, rhs):
        return self.data.__eq__(self.getdata(rhs))

    def __ne__(self, rhs):
        return self.data.__ne__(self.getdata(rhs))

    def __lt__(self, rhs):
        return self.data.__lt__(self.getdata(rhs))

    def __le__(self, rhs):
        return self.data.__le__(self.getdata(rhs))

    def __gt__(self, rhs):
        return self.data.__gt__(self.getdata(rhs))

    def __ge__(self, rhs):
        return self.data.__ge__(self.getdata(rhs))

    def __add__(self, rhs):
        return FileString(self.name, self.line, self.offset,
                          self.data + self.getdata(rhs))

    def __iadd__(self, rhs):
        self.data += self.getdata(rhs)
        return self

    @staticmethod
    def getdata(rhs):
        return rhs.data if isinstance(rhs, FileString) else rhs


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
        return FileString(self.name, self.number, offset, self.line[offset])

    def putchar(self, char):
        self.pending.append(char)
        return self

    def readchar(self, count):
        chars = tuple(self.getchar() for _ in range(count))
        return (''.join(str(c) for c in chars if c), chars)

    def writechar(self, chars):
        self.pending += reversed(chars)
        return self

    def emptychar(self):
        return FileString(self.name, self.number, self.offset, '')

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


class TokenType(Enum):
    EOL = 1
    SPACE = 2
    COMMENT = 3
    WORD = 4
    STRING = 5
    COLON = 6
    OPDEL = 7
    BRACKET_OPEN = 8
    BRACKET_CLOSE = 9
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
        return str(self.data)

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

    def getline(self):
        if isinstance(self.data, FileString):
            return self.data.line
        return -1


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
        '!': {'!=': {}},
        '$': {},
        '%': {'=': {}},
        '&': {'=': {}},
        '(': {},
        ')': {},
        '*': {'*': {'=': {}}, '=': {}},
        '+': {'=': {}},
        ',': {},
        '-': {'=': {}, '>': {}},
        '.': {'.': {'.': {}}},
        '/': {'/': {'=': {}}, '=': {}},
        ':': {'=': {}},
        ';': {},
        '<': {'<': {'=': {}}, '=': {}},
        '=': {'=': {}},
        '>': {'=': {'=': {}}, '>': {}},
        '?': {},
        '@': {'=': {}},
        '[': {},
        '\\': {'\n': {}},
        ']': {},
        '^': {'=': {}},
        '`': {},
        '{': {},
        '|': {'=': {}},
        '}': {},
        '~': {},
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
        self.char_type.update({c: self.getword for c in self.WORD_CHAR})
        self.char_type.update({c: self.getopdel for c in self.OPDELS})

        self.space_func = {chr(c): self.gettoken_char for c in range(33) if c != 10}
        self.space_func.update({'\\': self.getspace_escape})

        self.word_func = {c: self.gettoken_char for c in self.WORD_CHAR}
        self.word_func.update({'\x80': self.gettoken_char})

        self.getline_func = {
            TokenType.EOL: self.getline_eol,
            TokenType.BRACKET_OPEN: self.getline_bracket_open,
            TokenType.BRACKET_CLOSE: self.getline_bracket_close,
        }

        self.getopdel_func = {':': self.getopdel_colon, '\\\n': self.getspace}
        self.getopdel_func.update({c: self.getopdel_open for c in self.BRACKET_OPEN})
        self.getopdel_func.update({c: self.getopdel_close for c in self.BRACKET_CLOSE})

        self.debug = option.get('debug', False)
        self.tab_width = option.get('tab', DEFAULT_TAB_WIDTH)

        self.stream = stream
        self.getchar = stream.getchar
        self.putchar = stream.putchar
        self.readchar = stream.readchar
        self.writechar = stream.writechar

        self.errors = []
        self.gettoken_token = None

        self.getline_stack = []
        self.getline_error = False
        self.getline_tokens = []

    def seterror(self, err):
        self.errors.append(err)
        return self

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
        char = self.getchar()
        return self.char_type[min(char, '\x7F')](char) if char else None

    def geteol(self, char):
        return Token(TokenType.EOL, char)

    def gettoken_end(self, char):
        self.putchar(char)
        return False

    def gettoken_char(self, char):
        self.gettoken_token += char
        return True

    def getspace(self, space):
        self.gettoken_token = space
        fstop = self.gettoken_end
        for char in iter(self.getchar, None):
            if not self.space_func.get(char, fstop)(char):
                break
        return Token(TokenType.SPACE, self.gettoken_token)

    def getspace_escape(self, char):
        next_char = self.getchar()
        if next_char is None:
            self.gettoken_token += char
            return False
        if ord(next_char) < 33:
            self.gettoken_token += char + next_char
            return True
        self.writechar((char, next_char))
        return False

    def getword(self, word):
        self.gettoken_token = word
        fstop = self.gettoken_end
        for char in iter(self.getchar, None):
            if not self.word_func.get(min(char, '\x80'), fstop)(char):
                break
        return Token(TokenType.WORD, self.gettoken_token)

    def getquote(self, qstr):
        qch = str(qstr)
        for char in iter(self.getchar, None):
            if char == '\n':
                self.putchar(char)
                break
            qstr += char
            if char == qch:
                break
            if char != '\\':
                continue
            char = self.getchar()
            if char is None:
                break
            qstr += char
        return Token(TokenType.STRING, qstr)

    def getdquote(self, char):
        next_str, next_chars = self.readchar(2)
        if next_str == '""':
            return self.getlongquote(char + next_str)
        self.writechar(next_chars)
        return self.getquote(char)

    def getlongquote(self, qstr):
        for char in iter(self.getchar, None):
            if char == '"':
                next_str, next_chars = self.readchar(2)
                if next_str == '""':
                    qstr += '"""'
                    break
                self.writechar(next_chars)
            qstr += char
        return Token(TokenType.STRING, qstr)

    def getcomment(self, cstr):
        for char in iter(self.getchar, None):
            if char == '\n':
                self.putchar(char)
                break
            cstr += char
        return Token(TokenType.COMMENT, cstr)

    def getopdel(self, char):
        opdel = char
        cmap = self.OPDELS[char]
        if cmap:
            nchar = None
            for nchar in iter(self.getchar, None):
                opdelc = opdel + nchar
                cmap = cmap.get(opdelc)
                if not cmap:
                    break
                opdel = opdelc
            if nchar:
                self.putchar(nchar)
        return self.getopdel_func.get(opdel, self.getopdel_opdel)(opdel)

    def getopdel_opdel(self, opdel):
        return Token(TokenType.OPDEL, opdel)

    def getopdel_open(self, opdel):
        return Token(TokenType.BRACKET_OPEN, opdel)

    def getopdel_close(self, opdel):
        return Token(TokenType.BRACKET_CLOSE, opdel)

    def getopdel_colon(self, opdel):
        return Token(TokenType.COLON, opdel)

    def getline(self):
        self.getline_stack = []
        self.getline_tokens = []
        self.getline_error = False
        for token in iter(self.gettoken, None):
            self.getline_tokens.append(token)
            func = self.getline_func.get(token.type)
            if func and not func(token):
                break
        if not self.getline_tokens:
            return None
        if self.getline_error:
            self.getline_tokens.insert(0, Token(TokenType.ERROR))
        return self.getline_tokens

    def getline_eol(self, _token):
        return self.getline_stack

    def getline_bracket_open(self, token):
        self.getline_stack.append(self.BRACKET_OPEN[token.data])
        return True

    def getline_bracket_close(self, token):
        if not self.getline_stack or token.data != self.getline_stack.pop():
            self.seterror((token.data, "error: unmatched parentheses/bracket"))
            self.getline_error = True
        return True


class Parser(Lexer):
    def __init__(self, stream, **option):
        super().__init__(stream, **option)
        self.debug_line = option.get('debug_line', False)
        self.debug_append = option.get('debug_append', False)
        self.debug_remove = option.get('debug_remove', False)

        self.def_block_end = option.get('defend', 'pass')
        self.loop_block_end = option.get('loopend', 'pass')
        self.classdefnl = option.get('classdefnl', False)

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
        error = False
        if token == TokenType.ERROR:
            tokens.pop(0)
            token = tokens[0]
            error = True
        status = LineStatus(tokens, token.getline())
        status.error = error
        if token == TokenType.SPACE:
            status.indent = self.getcolumn(token.data)
            self.add_indent(status.indent, token.data)
            token = None if len(tokens) < 2 else tokens[1]
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
        if self.error:
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
        if not line.error and (
                block.indent_block < line.indent or not line.keyword_end):
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

        if not line.error and self.classdefnl:
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
                block_keyword = block.keyword.data
                line_keyword = line.keyword_end.data
                if (block_keyword not in self.remove_keyword_map[line_keyword]
                        or line_keyword != line.getline().strip()):
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

        if parser.errors and (args.debug or args.error_stop):
            for error in parser.errors:
                sys.stderr.write(f'{error[0].name}:{error[0].line}: {error[1]}\n')
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
