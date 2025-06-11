# -*- mode: snippet -*-
# name: gmake
# key: gmake
# expand-env: ((yas-indent-line 'fixed))
# --
CXX = g++
CXXFLAGS = -I/./cpp_comm/include -g -Wall -Wextra
LDLIBS = 
tgt = ${0:main}
dep = $1

\$(tgt): \$(dep).cpp
	mkdir -p build
	\$(CXX) \$(CXXFLAGS) -o build/\$@ $^ \$(LDLIBS)
