.class Simple
.super java/lang/Object

.method <init>()V
.limit stack 1
.limit locals 1
.line 1
aload_0
invokespecial java/lang/Object/<init>()V
return
.end method

.method public static main([Ljava/lang/String;)V

.limit stack 3
.limit locals 2
iconst_2
iconst_4
swap
isub
iconst_3
iadd
iconst_2
iadd
iconst_1
iadd
getstatic java/lang/System/out Ljava/io/PrintStream;
swap
invokevirtual java/io/PrintStream/println(I)V
iconst_2
istore_1
iconst_3
iload_1
iadd
getstatic java/lang/System/out Ljava/io/PrintStream;
swap
invokevirtual java/io/PrintStream/println(I)V
return
.end method
