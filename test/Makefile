
.PHONY: all clean

TARGETS = ref_counted_test  reader_test  edit_line_test environment_test evaluation_test
all: $(TARGETS)

# GDB でデバグする場合, `-g` オプションよりも `-g3` のほうが便利.
CXXFLAGS = -Wall -Wextra -Wno-unused-parameter -Wno-format-extra-args -g3

ref_counted_test: ref_counted_test.o
	$(CXX) $^ $(LDFLAGS) $(LDLIBS) -lstdc++ -licuuc -licuio -o $@

ref_counted_test.o: ref_counted_test.cpp ../ref_counted.h ../my_debug.h

reader_test: reader_test.o ../object_print.o ../reader.o
	$(CXX) $^ $(LDFLAGS) $(LDLIBS) -lstdc++ -licuuc -licuio -o $@


edit_line_test: edit_line_test.o ../edit_line.o
	$(CXX) $^ $(LDFLAGS) $(LDLIBS) -lstdc++ -licuuc -licuio -ledit -o $@


environment_test: environment_test.o ../environment.o ../object_print.o
	$(CXX) $^ $(LDFLAGS) $(LDLIBS) -lstdc++ -licuuc -licuio -ledit -o $@

evaluation_test: evaluation_test.o ../evaluation.o ../environment.o ../object_print.o
	$(CXX) $^ $(LDFLAGS) $(LDLIBS) -lstdc++ -licuuc -licuio -ledit -o $@


../reader.o: ../reader.cpp ../s_expr.h 
../object_print.o: ../object_print.cpp ../s_expr.h 
../s_expr.h: ../my_debug.h
../environment.o: ../environment.cpp ../environment.h ../s_expr.h
../evaluation.o: ../evaluation.cpp ../environment.h ../s_expr.h


clean:
	rm -f ../*.o *.o $(TARGETS)

