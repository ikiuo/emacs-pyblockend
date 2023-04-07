# Python のブロック終端処理

Emacs で TAB キー (indent-for-tab-command) を多用する人向けのツールです。

## TAB キーでインデントが深くなっていく…

次のソース(※1)

```python:
class A:
    def __init__(self):
        self.data = []

    def append(self, value):
        self.data.append(value)


class B:
    def __init__(self):
        self.data = 0

    def add(self, value):
        self.data += value
```

を Emacs で開いて各行で TAB キーを押したとき

```python:
class A:
    def __init__(self):
        self.data = []

        def append(self, value):
            self.data.append(value)


            class B:
                def __init__(self):
                    self.data = 0

                    def add(self, value):
                        self.data += value
```

となってしまいます。（古い Emacs では <code>M-x indent-region</code> でも同様でした...）

そこで、ソース(※1) にブロック終端を明示して(※2)

```python:
class A:
    def __init__(self):
        self.data = []
        return

    def append(self, value):
        self.data.append(value)
        return

    pass


class B:
    def __init__(self):
        self.data = 0
        return

    def add(self, value):
        self.data += value
        return

    pass
```

の状態を維持すると、TAB キーを押したときの動作が(ブロック範囲が明確な)他の言語モードと大差がなくなります。


## 欲しい機能

自動的な処理により、次の状況になれば少しはマシになるかもしれません。

1. ファイルを開くとブロック終端なし(※1)でもブロック終端あり(※2)の状態になる
2. 保存するファイルはブロック終端あり(※2)からブロック終端なし(※1)に変換される
3. エディタでの編集はブロック終端あり(※2)のまま継続する


# ブロック終端の追加・削除ツール (pyblockend.py)

以下の処理を行います。

- ブロック終端なし(※1) から pass/continue/return を自動追加
- ブロック終端あり(※2) から pass/continue/return を自動削除

ブロック終端の追加では pass 以外に

- def ブロックでは return
- for, while ブロックでは continue

を選択可能です。

ブロック終端の削除では pass/continue/return の行に空白とキーワード以外があると削除されません。


# Emacs : py-blockend マイナーモード

.emacs で py-blockend.el の読み込みとフックの追加すると Python モードのマイナーモードが追加されます。

```lisp:
(add-hook 'python-mode-hook 'py-blockend-mode)
```

pyblockend.py を pyblockend コマンドとして呼び出せるようにするか、カスタマイズ変数 <code>py-blockend-command</code> を変更してください。


| コマンド | 内容 |
|:--|:--|
| M-x py-blockend-mode | py-blockend-mode マイナーモードの有無の切り替え |
| M-x py-blockend-append-buffer | バッファ内のブロック終端の追加 |
| M-x py-blockend-remove-buffer | バッファ内のブロック終端の削除 |
| M-x py-blockend-append-region | 選択範囲中のブロック終端の追加 |
| M-x py-blockend-remove-region | 選択範囲中のブロック終端の削除 |
| M-x py-blockend-toggle-append-after-save | 保存後処理のブロック終端追加処理の有無を切り替え |
| M-x py-blockend-toggle-global | py-blockend-mode のグローバル フラグを切り替え |
| M-x py-blockend-global-enable | py-blockend-mode のグローバル フラグを有効 |
| M-x py-blockend-global-disable | py-blockend-mode のグローバル フラグを無効 |

※グローバル フラグの操作は既に開いているファイル（バッファ）には影響しません。

| カスタマイズ変数 | 内容 | 既定 |
|:--|:--|:--|
| py-blockend-global | py-blockend-mode のグローバル フラグ | t |
| py-blockend-command | pyblockend.py を実行できるコマンド | pyblockend |
| py-blockend-command-append | ブロック終端追加用 pyblockend オプション | --append <br> --def-return |
| py-blockend-command-remove | ブロック終端削除用 pyblockend オプション | --remove |
| py-blockend-append-after-save | ファイル保存直後のブロック終端追加処理 | t |

def ブロックの終端を return にするには "--def-return" を、
for,while ブロックの終端を continue にするには "--loop-continue" を
py-blockend-command-append に追加してください。


### ファイルを開いた直後に "undo" する

ファイルを開いた直後に <code>M-x undo</code> できます。ブロック終端の追加前の状態になるので、保存されているファイルの状態を確認できます。

pyblockend の失敗により壊れたときの対処方法にもなります。


# 既知の問題

ブロック終端の変化により、カーソルが元の位置から移動することが屢々あります...
