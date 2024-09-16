
# my-lisp -- A small Lisp interpreter

非常に小さな Lisp インタプリタ。

参考:
<a href="https://github.com/kanaka/mal/blob/master/process/guide.md">The Make-A-Lisp Process</a>
ただ, MAL は Clojure に近く、Lisp らしい感じとは若干異なる。

Common Lisp か Scheme が有力。とりあえず Common Lisp のサブセットを実装。

C++17



 - REPL Read-Eval-Print-Loop
   + ✅ GNU Readline はライセンスが GPLv3 なので使えない。libedit-devel を利用
   + `repl()`, `main()`
     
 - Reader, Print  -- ファイルからの読み込み `READ()` 関数, S式の構築、画面への表示 `PRINT()` 関数. 文法をユーザが拡張できるので, bison/flex は使えない。手書き。
   + ✅ Standard Macro Characters
   + ✅ dotted pair notation
   + reader macro
   + Sharpsign (dispatching macro character)
     - `simple-vector`
   + 複数行コメント
   
 - Environment
   + ✅ 変数定義. `SETQ`
   + ✅ レキシカルスコープ. `LET`, `LET*`
   + ✅ ビルトイン関数の登録
   
 - Eval  -- `EVAL()` が AST を評価.
   + ✅ 関数呼び出し. Function `FUNCALL`
   + ✅ atom の評価
   + ✅ `lambda` 式からクロージャをつくる
   + ✅ ユーザ関数の定義 `DEFUN`
   + ✅ Special operator `IF`
   + macro `DO`
   + ✅ Tail Call Optimization (TCO)
 
 - Files
 
 - ✅ Quoting `QUOTE`
 
 - Macros
   + ✅ Backquote `quasiquote`, Comma `unquote`, `,@` `unquote-splicing`
   + ✅ Function `MACROEXPAND-1`. マクロの入れ子は未了
   + ✅ マクロの評価
   
 - Try (Conditions)
   + `tagbody` & 無条件ジャンプ `go`  -- 実装しない
   + `block` & `return-from`  -- lexical non-local exit facility
   + `catch` & `throw`   -- ほかのプログラミング言語の例外処理とは異なる。実装しない
   + `handler-bind` or `handler-case`  -- これが例外処理

 - 構造体
 
 - 若干のライブラリ
   + type `hash-table`. `(make-hash-table)`


