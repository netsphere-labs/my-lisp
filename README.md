
# my-lisp -- A small Lisp interpreter

非常に小さな Lisp インタプリタ。Common Lisp か Scheme が有力。
 - とりあえず Common Lisp のサブセットを実装。
 - S式の構築部分だけ, 独立して使える。

C++17
 - `variant` で数値は特別扱い
 - オブジェクト参照は `shared_ptr` で参照カウント方式
 - リストは `vector`
 - 文字列は全部 Unicode (UTF-16).

行数:
Language|files|blank|comment|code
:-------|-------:|-------:|-------:|-------:
C++|17|363|275|1215
C/C++ Header|6|225|234|507
Markdown|1|33|0|91
make|2|30|12|66
YAML|2|5|1|32
Lisp|2|5|4|12
SUM:|30|661|526|1923


## 機能

 - REPL Read-Eval-Print-Loop
   + ✅ GNU Readline はライセンスが GPLv3 なので使えない。libedit-devel を利用
   + ✅ `repl()`, `main()`
     
 - Reader, Print  -- ファイルからの読み込み `READ()` 関数, S式の構築、画面への表示 `PRINT()` 関数. 文法をユーザが拡張できるので, bison/flex は使えない。手書き。
   + ✅ Standard Macro Characters
   + ✅ dotted pair notation
   + reader macro
   + Sharpsign (dispatching macro character)
     - `<simple-vector>`
   + ✅ 単一行コメント `;`
   + 複数行コメント `#|`...`|#`
   
 - Environment
   + ✅ 変数定義. `SETQ`
   + ✅ レキシカルスコープ. `LET`, `LET*`. `BLOCK` ではスコープ導入しない。
   + ✅ ビルトイン関数の登録
   
 - Eval  -- `EVAL1()` が AST を評価.
   + ✅ 関数呼び出し. 
   + ✅ atom の評価
   + ✅ `lambda` 式からクロージャをつくる
   + Function `FUNCALL`. 変数を評価して, クロージャの呼び出し. Lisp-2
   + ✅ ユーザ関数の定義 `DEFUN`
   + ✅ Special operator `IF`
   + macro `DO`
   
 - ✅ Tail Call Optimization (TCO). トランポリン trampoline で相互再帰もOK.
 
 - Files
 
 - ✅ Quoting `QUOTE`
 
 - Macros
   + ✅ Backquote (`quasiquote`), Comma (`unquote`), `,@` (`unquote-splicing`)
   + ✅ Function `MACROEXPAND-1`.
   + マクロの入れ子
   + ✅ マクロの評価
   
 - Try (Conditions)
   + `tagbody` & 無条件ジャンプ `go`  -- 実装しない
   + ✅ `block` & `return-from`  -- lexical non-local exit facility. `StopIteration` 例外を投げる
   + `catch` & `throw`   -- ほかのプログラミング言語の例外処理とは異なる。実装しない
   + `handler-bind` or `handler-case`  -- これが例外処理

 - 構造体
 
 - 若干のライブラリ
   + ビルトイン関数
     - `<string>` は `<character>` の vector *にしない*.
   + type `hash-table`. `(make-hash-table)`




## Data Type Predicates の命名規則

Common Lisp は, クラス名に `-` を含まなければ `P` を, そうでなければ `-P` を末尾に付ける。そうすると, `null` と `atom` には `P` を付けないといかんのでは?

|CL              |Scheme      |type          |                                |
|----------------|------------|--------------|--------------------------------|
|                |boolean?    |              | Scheme のみ                    |
|`null`          |null?       |null          | == (eq x '())                  | 
|SYMBOLP         |symbol?     |symbol        | nil は真.                      |
| `atom`         |            |atom          | NIL は真.  == (not (typep x 'cons))   |
|`consp`         |pair?       |cons          | nil は偽.  == (not (typep x 'atom))  |
|`listp`         |list?       |list          | '() は真. == (typep x '(or cons null))  |
|NUMBERP         |number?     |number        |                                |
|`integerp`      |integer?    |integer       |                                |
|`rationalp`     |            |rational      |                                |
|`floatp`        |            |float         |                                |
|`complexp`      |            |complex       |                                |
|CHARACTERP      |char?       |character     |                                |
|STRINGP         |string?     |string        |                                |
|bit-vector-p    |            |bit-vector    | `bit-vector` means `(vector bit)`. |
|VECTORP         |vector?     |vector        |                                |
|SIMPLE-VECTOR-P |            |simple-vector |                                |
|simple-string-p |            |simple-string |                                |
|simple-bit-vector-p|         |simple-bit-vector |                            |
|arrayp          |            |array         |                                |
|packagep        |            |PACKAGE       | 名前空間                       |
|FUNCTIONP       |procedure?  |FUNCTION      | マクロもクロージャも真         |
|compiled-function-p |        |compiled-function |                            |
|STREAMP         |port?, eof-object? |STREAM     |Scheme: <port> を介して入出力. EOF のときは EOFオブジェクトを返す.  |
|random-state-p  |            |RANDOM-STATE  |                                |
|readtablep      |            |READTABLE     |                                |
|hash-table-p    |            |HASH-TABLE    |                                |
|pathnamep       |            |PATHNAME      |                                |
|                |bytevector? |              |                                |



## 参考

<a href="https://github.com/kanaka/mal/blob/master/process/guide.md">The Make-A-Lisp Process</a>
ただ, MAL は Clojure に近く、Lisp らしい感じとは若干異なる。


そのほか興味深い:

#### <a href="https://github.com/seven1m/malcc/">seven1m/malcc: Mal (Make A Lisp) Compiler in C</a>
Lisp を C言語にコンパイル。後は gcc でバイナリを作れる。

そのままではビルドに失敗する。`__malloc_hook` undeclared. 組み込みの tinycc を `dev` ブランチに切り替えてやれば動く。

出力されるCのソースは, それぞれの関数末尾で継続を返すスタイルになっている。

#### <a href="https://github.com/rui314/minilisp/">rui314/minilisp: A readable lisp in less than 1k lines of C</a>

1,000行に満たないのに GC 付き。すごい。


