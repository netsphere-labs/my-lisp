
.PHONY: all clean

TARGETS = ref_counted_test  reader_test  edit_line_test environment_test \
    macro_test evaluation_test block_test

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

ref_counted_test: ref_counted_test.o
	$(CXX) $^ $(LDFLAGS) $(LDLIBS) -lstdc++ -licuuc -licuio -o $@

ref_counted_test.o: ref_counted_test.cpp ../ref_counted.h ../my_debug.h

reader_test: reader_test.o ../object_print.o ../reader.o
	$(CXX) $^ $(LDFLAGS) $(LDLIBS) -lstdc++ -licuuc -licuio -o $@


edit_line_test: edit_line_test.o ../edit_line.o
	$(CXX) $^ $(LDFLAGS) $(LDLIBS) -lstdc++ -licuuc -licuio -ledit -o $@


environment_test: environment_test.o ../environment.o ../object_print.o ../reader.o ../evaluation.o ../macros.o
	$(CXX) $^ $(LDFLAGS) $(LDLIBS) -lstdc++ -licuuc -licuio -ledit -o $@

macro_test: macro_test.o ../macros.o ../environment.o ../object_print.o ../reader.o ../evaluation.o ../builtin-functions.o
	$(CXX) $^ $(LDFLAGS) $(LDLIBS) -lstdc++ -licuuc -licuio -ledit -o $@

evaluation_test: evaluation_test.o ../evaluation.o ../environment.o ../object_print.o ../reader.o ../macros.o ../builtin-functions.o
	$(CXX) $^ $(LDFLAGS) $(LDLIBS) -lstdc++ -licuuc -licuio -ledit -o $@

block_test: block_test.o ../evaluation.o ../environment.o ../object_print.o ../reader.o ../macros.o ../builtin-functions.o
	$(CXX) $^ $(LDFLAGS) $(LDLIBS) -lstdc++ -licuuc -licuio -ledit -o $@

../reader.o: ../reader.cpp ../s_expr.h 
../object_print.o: ../object_print.cpp ../s_expr.h 
../s_expr.h: ../my_debug.h
../environment.o: ../environment.cpp ../environment.h ../s_expr.h
../evaluation.o: ../evaluation.cpp ../environment.h ../s_expr.h
../builtin-functions.o: ../builtin-functions.cpp ../environment.h ../s_expr.h

clean:
	rm -f ../*.o *.o $(TARGETS)

