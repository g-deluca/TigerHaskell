.data
L9:
.long 8
.string "arr[1]: "
L8:
.long 1
.string "
"
L7:
.long 8
.string "arr[0]: "
L5:
.long 7
.string "newells"
L2:
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

L11:

push %ebx

push %ecx

mov $L2, %eax

push %eax

mov $10, %eax

push %eax

call _allocArray

add $8, %esp

pop %ebx

pop %ecx

mov %eax, %edi

mov $L5, %eax

mov $0, %esi

mov $4, %edx

imul %esi, %edx

add %edi, %edx

mov %eax, (%edx)

push %ebx

push %ecx

mov $L7, %eax

push %eax

call print

add $4, %esp

pop %ebx

pop %ecx

push %ebx

push %ecx

mov $0, %ebx

mov $4, %eax

imul %ebx, %eax

add %edi, %eax

mov (%eax), %eax

push %eax

call print

add $4, %esp

pop %ebx

pop %ecx

push %ebx

push %ecx

mov $L8, %eax

push %eax

call print

add $4, %esp

pop %ebx

pop %ecx

push %ebx

push %ecx

mov $L9, %eax

push %eax

call print

add $4, %esp

pop %ebx

pop %ecx

push %ebx

push %ecx

mov $1, %ebx

mov $4, %eax

imul %ebx, %eax

add %edi, %eax

mov (%eax), %eax

push %eax

call print

add $4, %esp

pop %ebx

pop %ecx

jmp L10

L10:

 
add $0, %esp

pop %ebp

pop %ebx

pop %esp

pop %esi

pop %edi

pop %ebp

ret


