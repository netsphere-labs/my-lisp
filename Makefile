
.PHONY: all clean

TARGETS = mylisp

all: $(TARGETS)

# コンパイラオプション:
# https://best.openssf.org/Compiler-Hardening-Guides/Compiler-Options-Hardening-Guide-for-C-and-C++
# 英語版のほうが更新されている。

# GDB でデバグする場合, `-g` オプションよりも `-g3` のほうが便利.
# リリースビルド = -DNDEBUG
DEBUG = -g3 -D_DEBUG

# _FORTIFY_SOURCE は副作用がありうる
CXXFLAGS = -Wall -Wextra -Wno-unused-parameter -Wno-format-extra-args \
    -O2 -Wformat -Wformat=2 -Wimplicit-fallthrough -Werror=format-security \
    $(DEBUG) \
    -U_FORTIFY_SOURCE -D_FORTIFY_SOURCE=3 \
    -D_GLIBCXX_ASSERTIONS \
    -fstrict-flex-arrays=3 \
    -fstack-clash-protection -fstack-protector-strong \
    -Wl,-z,nodlopen -Wl,-z,noexecstack \
    -Wl,-z,relro -Wl,-z,now \
    -Wl,--as-needed -Wl,--no-copy-dt-needed-entries \
    -fPIE -pie

mylisp: main.o edit_line.o reader.o object_print.o environment.o evaluation.o macros.o builtin-functions.o
	$(CXX) $^ $(LDFLAGS) $(LDLIBS) -lstdc++ -licuuc -licuio -ledit -o $@

reader.o: reader.cpp s_expr.h 
object_print.o: object_print.cpp s_expr.h 
s_expr.h: my_debug.h
environment.o: environment.cpp environment.h s_expr.h
evaluation.o: evaluation.cpp environment.h s_expr.h
builtin-functions.o: builtin-functions.cpp environment.h s_expr.h

clean:
	rm -f *.o $(TARGETS)

