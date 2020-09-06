.data
L8:
.long 4
.string "chau"
L7:
.long 4
.string "hola"

.text
.globl _tigermain

.type _tigermain, @function

_tigermain:

push %ebp

mov %esp, %ebp

push %edi

push %esi

push %esp

push %ebx

push %ebp

sub $0, %esp

L14:

mov $10, %eax

mov %eax, %edi

mov $20, %eax

cmp %eax, %edi

jg L3

jmp L4

L4:

mov $0, %eax

L5:

mov $0, %edi

cmp %edi, %eax

jne L9

jmp L10

L10:

push %ebx

push %ecx

mov $L8, %eax

push %eax

call print

add $4, %esp

pop %ebx

pop %ecx

mov $0, %eax

L11:

jmp L13

L3:

mov $1, %eax

jmp L5

L9:

push %ebx

push %ecx

mov $L7, %eax

push %eax

call print

add $4, %esp

pop %ebx

pop %ecx

mov $0, %eax

jmp L11

L13:

 
add $0, %esp

pop %ebp

pop %ebx

pop %esp

pop %esi

pop %edi

pop %ebp

ret


