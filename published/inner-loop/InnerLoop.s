	.def	 _InnerLoop_zdwgo_info;
	.scl	2;
	.type	32;
	.endef
	.section	X98A__STRIP,__me2,"xr"
	.globl	_InnerLoop_zdwgo_info
	.align	4, 0x90
_InnerLoop_zdwgo_info:                  # @InnerLoop_zdwgo_info
# BB#0:                                 # %c1Ue
	subl	$44, %esp
	movl	%ebx, 40(%esp)
	movl	%ebp, 36(%esp)
	movl	%edi, 32(%esp)
	movl	%esi, 28(%esp)
	movl	36(%esp), %eax
	movl	(%eax), %eax
	movzbl	(%eax), %eax
	cmpl	$37, %eax
	sbbl	%ecx, %ecx
	andl	$1, %ecx
	cmpl	$37, %eax
	movl	%eax, 24(%esp)
	movl	%eax, 20(%esp)
	movl	%eax, 16(%esp)
	movl	%ecx, 12(%esp)
	jb	LBB0_3
# BB#1:                                 # %n200
	movl	36(%esp), %eax
	movl	(%eax), %eax
	incl	%eax
	movl	%eax, 8(%esp)
LBB0_2:                                 # %n200
	movl	36(%esp), %ecx
	movl	%eax, (%ecx)
	movl	28(%esp), %esi
	movl	32(%esp), %edi
	movl	36(%esp), %ebp
	movl	40(%esp), %ebx
	addl	$44, %esp
	jmp	_InnerLoop_zdwgo_info   # TAILCALL
LBB0_3:                                 # %c1Uh
	movl	16(%esp), %eax
	movl	%eax, 4(%esp)
	cmpl	$13, %eax
	jb	LBB0_11
# BB#4:                                 # %n20g
	cmpl	$32, 4(%esp)
	jb	LBB0_9
# BB#5:                                 # %n20j
	cmpl	$36, 4(%esp)
	jb	LBB0_8
# BB#6:                                 # %n20m
	cmpl	$36, 4(%esp)
	je	LBB0_13
LBB0_7:                                 # %c1Ut
	movl	36(%esp), %eax
	movl	(%eax), %eax
	incl	%eax
	movl	%eax, (%esp)
	jmp	LBB0_2
LBB0_8:                                 # %c1Us
	cmpl	$32, 4(%esp)
	jne	LBB0_7
	jmp	LBB0_13
LBB0_9:                                 # %c1Ur
	cmpl	$13, 4(%esp)
	jne	LBB0_7
	jmp	LBB0_13
LBB0_10:                                # %c1Uu
	cmpl	$0, 4(%esp)
	jne	LBB0_7
	jmp	LBB0_13
LBB0_11:                                # %c1Uq
	cmpl	$10, 4(%esp)
	jb	LBB0_10
# BB#12:                                # %n21T
	cmpl	$10, 4(%esp)
	jne	LBB0_7
LBB0_13:                                # %n20p
	movl	36(%esp), %eax
	movl	(%eax), %eax
	movl	%eax, 28(%esp)
	movl	36(%esp), %eax
	leal	4(%eax), %ebp
	movl	%ebp, 36(%esp)
	movl	4(%eax), %eax
	movl	28(%esp), %esi
	movl	32(%esp), %edi
	movl	40(%esp), %ebx
	addl	$44, %esp
	jmpl	*%eax                   # TAILCALL


