extern U0 Print(U8 *fmt,...);

Print("%z|%5z|%z\n",3,"AL\0AX\0EAX\0RAX\0",
      1,"A\0@x\0B\0",-1,"unused");
42;
