Write a Shake example, following the user manual, as a blog post.

```
(Linux)
g++ -dynamic -g -c -arch i386 x86_64 -Wall -fPIC -I/Library/Java/JavaVirtualMachines/jdk1.8.0_60.jdk/Contents/Home/include/darwin -I./include/iConnect -I./include/TsiSdk -I./include/TsiSdk++ -I./lib iConnectJavaJCPP.cpp -o iConnectJCPP.o

g++ -dynamiclib -arch i386 x86_64 -Wl,-soname,libiConnectJCPP.so, --no-undefined -L./lib iConnectJCPP.o -o libiConnectJCPP.dylib -lTsiSdk -liConnect -lTsiSdk++

(Mac OS X)

g++ -dynamic -g -c -arch i386 x86_64 -Wall -I/Library/Java/JavaVirtualMachines/jdk1.8.0_60.jdk/Contents/Home/include/darwin -I./include/iConnect -I./include/TsiSdk -I./include/TsiSdk++ -I./lib iConnectJavaJCPP.cpp -o iConnectJCPP.o


g++ -dynaniclib -arch i386 x86_64 -undefined suppress -flat_namespace *.o *.a -o libiconnectjcpp.dylib
```

Note: difference between Linux and Mac OS X is the -fPIC switch (position independent code, which is not valid for MachO dynamic library) and linking flat namespace (Mac OS X is default to 2-tiered).
