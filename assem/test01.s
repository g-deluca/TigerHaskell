.data
L9:
.long 10
.string "No es cero"
L8:
.long 7
.string "Es cero"

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

L15:

push %ebx

push %ecx

mov $3, %eax

push %eax

mov $10, %eax

push %eax

call _allocArray

add $8, %esp

pop %ebx

pop %ecx

mov $-20, %edi

add %ebp, %edi

mov %eax, (%edi)

mov -20(%ebp), %eax

mov -20(%ebp), %edx

mov $0, %edi

mov $4, %eax

imul %edi, %eax

add %edx, %eax

mov (%eax), %edi

mov $0, %eax

cmp %eax, %edi

je L4

jmp L5

L5:

mov $0, %eax

L6:

mov $0, %edi

cmp %edi, %eax

jne L10

jmp L11

L11:

push %ebx

push %ecx

mov $L9, %eax

push %eax

call print

add $4, %esp

pop %ebx

pop %ecx

mov $0, %eax

L12:

push %ebx

push %ecx

mov $42, %eax

push %eax

push %ebp

call g

add $8, %esp

pop %ebx

pop %ecx

jmp L14

L4:

mov $1, %eax

jmp L6

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

jmp L12

L14:

 
add $0, %esp

pop %ebp

pop %ebx

pop %esp

pop %esi

pop %edi

pop %ebp

ret

.globl g

.type g, @function

g:

push %ebp

mov %esp, %ebp

push %edi

push %esi

push %esp

push %ebx

push %ebp

sub $0, %esp

L17:

mov 8(%ebp), %eax

mov -20(%eax), %ecx

mov $0, %ebx

mov $4, %eax

imul %ebx, %eax

add %ecx, %eax

mov (%eax), %ebx

mov 12(%ebp), %eax

add %ebx, %eax

jmp L16

L16:

 
add $0, %esp

pop %ebp

pop %ebx

pop %esp

pop %esi

pop %edi

pop %ebp

ret


