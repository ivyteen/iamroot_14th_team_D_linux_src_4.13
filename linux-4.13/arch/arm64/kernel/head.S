/*
 * Low-level CPU initialisation
 * Based on arch/arm/kernel/head.S
 *
 * Copyright (C) 1994-2002 Russell King
 * Copyright (C) 2003-2012 ARM Ltd.
 * Authors:	Catalin Marinas <catalin.marinas@arm.com>
 *		Will Deacon <will.deacon@arm.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 2 as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <linux/linkage.h>
#include <linux/init.h>
#include <linux/irqchip/arm-gic-v3.h>

#include <asm/assembler.h>
#include <asm/boot.h>
#include <asm/ptrace.h>
#include <asm/asm-offsets.h>
#include <asm/cache.h>
#include <asm/cputype.h>
#include <asm/elf.h>
#include <asm/kernel-pgtable.h>
#include <asm/kvm_arm.h>
#include <asm/memory.h>
#include <asm/pgtable-hwdef.h>
#include <asm/pgtable.h>
#include <asm/page.h>
#include <asm/smp.h>
#include <asm/sysreg.h>
#include <asm/thread_info.h>
#include <asm/virt.h>

#include "efi-header.S"

#define __PHYS_OFFSET	(KERNEL_START - TEXT_OFFSET)

#if (TEXT_OFFSET & 0xfff) != 0
#error TEXT_OFFSET must be at least 4KB aligned
#elif (PAGE_OFFSET & 0x1fffff) != 0
#error PAGE_OFFSET must be at least 2MB aligned
#elif TEXT_OFFSET > 0x1fffff
#error TEXT_OFFSET must be less than 2MB
#endif

/*
 * Kernel startup entry point.
 * ---------------------------
 *
 * The requirements are:
 * MMU = off, D-cache = off, I-cache = on or off, 
 *   
 * 	 *iamroot 14차D팀*
 *	 *부팅시 memroy에 적재된 instruction set 실행을 할 수 있으므로 I-cache는 on or off
 *   *Doc/arm64/booting.txt내 'Caches, MMUs' 부분에 설명 추가.                        
 *   *cache coherency문제 떄문에 cache를 끈다.                                        
 *   
 * x0 = physical address to the FDT blob. 
 *   *iamroot 14차D팀*
 *   *U-boot에서 FDT PA를 x0로 넘겨준다.                                     
 *   *blob:binary large object                                               
 *   *FDT(Flattened Device Tree)=하드웨어의 구조를 기술하기 위한 데이터 구조.
 *   
 *
 * This code is mostly position independent so you call this at
 * __pa(PAGE_OFFSET + TEXT_OFFSET).
 *
 * Note that the callee-saved registers are used for storing variables
 * that are useful before the MMU is enabled. The allocations are described
 * in the entry routines.
 */
	__HEAD 			/*Iamroot14차D팀.head.text section 영역*/

_head:
	/*
	 * DO NOT MODIFY. Image header expected by Linux boot-loaders.
	 */
#ifdef CONFIG_EFI //EFI=executable firmware interface
	/*
	 * This add instruction has no meaningful effect except that
	 * its opcode forms the magic "MZ" signature required by UEFI.
	 */
	add	x13, x18, #0x16
	b	stext
#else
	b	stext				// branch to kernel start, magic
	.long	0				// reserved
#endif
	le64sym	_kernel_offset_le		// Image load offset from start of RAM, little-endian
	le64sym	_kernel_size_le			// Effective size of kernel image, little-endian
	le64sym	_kernel_flags_le		// Informative flags, little-endian
	.quad	0				// reserved
	.quad	0				// reserved
	.quad	0				// reserved
	.ascii	"ARM\x64"			// Magic number
#ifdef CONFIG_EFI
	.long	pe_header - _head		// Offset to the PE header.

pe_header:
	__EFI_PE_HEADER
#else
	.long	0				// reserved
#endif

	__INIT

	/*
	 * The following callee saved general purpose registers are used on the
	 * primary lowlevel boot path:
	 *
	 *  Register   Scope                      Purpose
	 *  x21        stext() .. start_kernel()  FDT pointer passed at boot in x0
	 *  x23        stext() .. start_kernel()  physical misalignment/KASLR offset
	 *  x28        __create_page_tables()     callee preserved temp register
	 *  x19/x20    __primary_switch()         callee preserved temp registers
	 */
ENTRY(stext)
	bl	preserve_boot_args
	bl	el2_setup			// Drop to EL1, w0=cpu_boot_mode
							// iamroot - EL2 : Hypervisor 
							// iamroot - w0 : x0의 32bit 값
	adrp	x23, __PHYS_OFFSET
	and	x23, x23, MIN_KIMG_ALIGN - 1	// KASLR offset, defaults to 0
	bl	set_cpu_boot_mode_flag
	bl	__create_page_tables
	/*
	 * The following calls CPU setup code, see arch/arm64/mm/proc.S for
	 * details.
	 * On return, the CPU will be ready for the MMU to be turned on and
	 * the TCR will have been set.
	 */
	bl	__cpu_setup			// initialise processor
	b	__primary_switch
ENDPROC(stext)

/*
 * Preserve the arguments passed by the bootloader in x0 .. x3
 * 		x0 = physical address to the FDT blob.
 */
preserve_boot_args:
	mov	x21, x0				// x21=FDT
							//	*iamroot 14차D팀*
							//	x0=FDT의 PA를 x21로 복사

	adr_l	x0, boot_args	// record the contents of 
							//	*iamroot 14차D팀*
							// boot_args 배열의 주소를 x0에 저장
							// boot_args는 arch/arm64/kernel/setup.c에서 아래와 같이 선언
							// u64 __cacheline_aligned boot_args[4];

	stp	x21, x1, [x0]		// x0 .. x3 at kernel entry
	stp	x2, x3, [x0, #16]	
							//	*iamroot 14차D팀*
							//	boot_args 배열에 bootloader에서 전달한 인자를 저장
							//	*x0~3중 x0에만 FDT PA가 들어가 있고, x1~3은 0으로 유지
							//	stp 명령은 2개의 register 내용을 메모리에 저장(store)
							//	[x0] : x0의 값을 통한 간접 참조
							//	register 1개가 64bit(8Byte)이므로, 두번째 stp 명령에서는 x0+16의 위치에 저장
							//	결국 아래와 같이 저장됨
							//	boot_args[0] = x21 (FDT의 physical address)
							// 	boot_args[1]~[3] = 0
							//	*후에 setup_arch()에서 x1~3 reg에 값이 존재하는지 확인 후 pr_err처리

	dmb	sy					// needed before dc ivac with
							// MMU off
							//*iamroot 14차D팀*
							//*Data Memory Barrier(DMB)
						    //*메모리 장벽 역할을 한다. 프로그램 순서에서 dmb 명령어 앞에 있는 
						   	//*모든 메모리 액세스 명령들은 dmb 명령어 뒤에 있는 
						   	//*메모리 액세스보다 먼저 처리되도록 한다. 
							// 어셈블러에 의한 최적화를 방지하기 위한 것으로 보임

	add	x1, x0, #0x20			// 4 x 8 bytes
								//*iamroot14차D팀* 
								//*x1 address = boot_args + offset 32(0x20)의 값을 가짐
								//*x21,x1,x2,x3의 값을 boot_args에 할당 했으므로 32의 offset을 가짐

	b	__inval_cache_range	// tail call
ENDPROC(preserve_boot_args)

/*
 * Macro to create a table entry to the next page.
 *
 *	tbl:	page table address
 *	virt:	virtual address
 *	shift:	#imm page table shift
 *	ptrs:	#imm pointers per table page
 *
 * Preserves:	virt
 * Corrupts:	tmp1, tmp2
 * Returns:	tbl -> next level table page address
 */
	.macro	create_table_entry, tbl, virt, shift, ptrs, tmp1, tmp2
	lsr	\tmp1, \virt, #\shift
	and	\tmp1, \tmp1, #\ptrs - 1	// table index
	add	\tmp2, \tbl, #PAGE_SIZE
	orr	\tmp2, \tmp2, #PMD_TYPE_TABLE	// address of next table and entry type
	str	\tmp2, [\tbl, \tmp1, lsl #3]
	add	\tbl, \tbl, #PAGE_SIZE		// next level table page
	.endm

/*
 * Macro to populate the PGD (and possibily PUD) for the corresponding
 * block entry in the next level (tbl) for the given virtual address.
 *
 * Preserves:	tbl, next, virt
 * Corrupts:	tmp1, tmp2
 */
	.macro	create_pgd_entry, tbl, virt, tmp1, tmp2
	create_table_entry \tbl, \virt, PGDIR_SHIFT, PTRS_PER_PGD, \tmp1, \tmp2
#if SWAPPER_PGTABLE_LEVELS > 3
	create_table_entry \tbl, \virt, PUD_SHIFT, PTRS_PER_PUD, \tmp1, \tmp2
#endif
#if SWAPPER_PGTABLE_LEVELS > 2
	create_table_entry \tbl, \virt, SWAPPER_TABLE_SHIFT, PTRS_PER_PTE, \tmp1, \tmp2
#endif
	.endm

/*
 * Macro to populate block entries in the page table for the start..end
 * virtual range (inclusive).
 *
 * Preserves:	tbl, flags
 * Corrupts:	phys, start, end, pstate
 */
	.macro	create_block_map, tbl, flags, phys, start, end
	lsr	\phys, \phys, #SWAPPER_BLOCK_SHIFT
	lsr	\start, \start, #SWAPPER_BLOCK_SHIFT
	and	\start, \start, #PTRS_PER_PTE - 1	// table index
	orr	\phys, \flags, \phys, lsl #SWAPPER_BLOCK_SHIFT	// table entry
	lsr	\end, \end, #SWAPPER_BLOCK_SHIFT
	and	\end, \end, #PTRS_PER_PTE - 1		// table end index
9999:	str	\phys, [\tbl, \start, lsl #3]		// store the entry
	add	\start, \start, #1			// next entry
	add	\phys, \phys, #SWAPPER_BLOCK_SIZE		// next block
	cmp	\start, \end
	b.ls	9999b
	.endm

/*
 * Setup the initial page tables. We only setup the barest amount which is
 * required to get the kernel running. The following sections are required:
 *   - identity mapping to enable the MMU (low address, TTBR0)
 *   - first few MB of the kernel linear mapping to jump to once the MMU has
 *     been enabled
 */
__create_page_tables:
	mov	x28, lr

	/*
	 * Invalidate the idmap and swapper page tables to avoid potential
	 * dirty cache lines being evicted.
	 */
	adrp	x0, idmap_pg_dir
	adrp	x1, swapper_pg_dir + SWAPPER_DIR_SIZE + RESERVED_TTBR0_SIZE
	bl	__inval_cache_range

	/*
	 * Clear the idmap and swapper page tables.
	 */
	adrp	x0, idmap_pg_dir
	adrp	x6, swapper_pg_dir + SWAPPER_DIR_SIZE + RESERVED_TTBR0_SIZE
1:	stp	xzr, xzr, [x0], #16
	stp	xzr, xzr, [x0], #16
	stp	xzr, xzr, [x0], #16
	stp	xzr, xzr, [x0], #16
	cmp	x0, x6
	b.lo	1b

	mov	x7, SWAPPER_MM_MMUFLAGS

	/*
	 * Create the identity mapping.
	 */
	adrp	x0, idmap_pg_dir
	adrp	x3, __idmap_text_start		// __pa(__idmap_text_start)

#ifndef CONFIG_ARM64_VA_BITS_48
#define EXTRA_SHIFT	(PGDIR_SHIFT + PAGE_SHIFT - 3)
#define EXTRA_PTRS	(1 << (48 - EXTRA_SHIFT))

	/*
	 * If VA_BITS < 48, it may be too small to allow for an ID mapping to be
	 * created that covers system RAM if that is located sufficiently high
	 * in the physical address space. So for the ID map, use an extended
	 * virtual range in that case, by configuring an additional translation
	 * level.
	 * First, we have to verify our assumption that the current value of
	 * VA_BITS was chosen such that all translation levels are fully
	 * utilised, and that lowering T0SZ will always result in an additional
	 * translation level to be configured.
	 */
#if VA_BITS != EXTRA_SHIFT
#error "Mismatch between VA_BITS and page size/number of translation levels"
#endif

	/*
	 * Calculate the maximum allowed value for TCR_EL1.T0SZ so that the
	 * entire ID map region can be mapped. As T0SZ == (64 - #bits used),
	 * this number conveniently equals the number of leading zeroes in
	 * the physical address of __idmap_text_end.
	 */
	adrp	x5, __idmap_text_end
	clz	x5, x5
	cmp	x5, TCR_T0SZ(VA_BITS)	// default T0SZ small enough?
	b.ge	1f			// .. then skip additional level

	adr_l	x6, idmap_t0sz
	str	x5, [x6]
	dmb	sy
	dc	ivac, x6		// Invalidate potentially stale cache line

	create_table_entry x0, x3, EXTRA_SHIFT, EXTRA_PTRS, x5, x6
1:
#endif

	create_pgd_entry x0, x3, x5, x6
	mov	x5, x3				// __pa(__idmap_text_start)
	adr_l	x6, __idmap_text_end		// __pa(__idmap_text_end)
	create_block_map x0, x7, x3, x5, x6

	/*
	 * Map the kernel image (starting with PHYS_OFFSET).
	 */
	adrp	x0, swapper_pg_dir
	mov_q	x5, KIMAGE_VADDR + TEXT_OFFSET	// compile time __va(_text)
	add	x5, x5, x23			// add KASLR displacement
	create_pgd_entry x0, x5, x3, x6
	adrp	x6, _end			// runtime __pa(_end)
	adrp	x3, _text			// runtime __pa(_text)
	sub	x6, x6, x3			// _end - _text
	add	x6, x6, x5			// runtime __va(_end)
	create_block_map x0, x7, x3, x5, x6

	/*
	 * Since the page tables have been populated with non-cacheable
	 * accesses (MMU disabled), invalidate the idmap and swapper page
	 * tables again to remove any speculatively loaded cache lines.
	 */
	adrp	x0, idmap_pg_dir
	adrp	x1, swapper_pg_dir + SWAPPER_DIR_SIZE + RESERVED_TTBR0_SIZE
	dmb	sy
	bl	__inval_cache_range

	ret	x28
ENDPROC(__create_page_tables)
	.ltorg

/*
 * The following fragment of code is executed with the MMU enabled.
 *
 *   x0 = __PHYS_OFFSET
 */
__primary_switched:
	adrp	x4, init_thread_union
	add	sp, x4, #THREAD_SIZE
	adr_l	x5, init_task
	msr	sp_el0, x5			// Save thread_info

	adr_l	x8, vectors			// load VBAR_EL1 with virtual
	msr	vbar_el1, x8			// vector table address
	isb

	stp	xzr, x30, [sp, #-16]!
	mov	x29, sp

	str_l	x21, __fdt_pointer, x5		// Save FDT pointer

	ldr_l	x4, kimage_vaddr		// Save the offset between
	sub	x4, x4, x0			// the kernel virtual and
	str_l	x4, kimage_voffset, x5		// physical mappings

	// Clear BSS
	adr_l	x0, __bss_start
	mov	x1, xzr
	adr_l	x2, __bss_stop
	sub	x2, x2, x0
	bl	__pi_memset
	dsb	ishst				// Make zero page visible to PTW

#ifdef CONFIG_KASAN
	bl	kasan_early_init
#endif
#ifdef CONFIG_RANDOMIZE_BASE
	tst	x23, ~(MIN_KIMG_ALIGN - 1)	// already running randomized?
	b.ne	0f
	mov	x0, x21				// pass FDT address in x0
	bl	kaslr_early_init		// parse FDT for KASLR options
	cbz	x0, 0f				// KASLR disabled? just proceed
	orr	x23, x23, x0			// record KASLR offset
	ldp	x29, x30, [sp], #16		// we must enable KASLR, return
	ret					// to __primary_switch()
0:
#endif
	b	start_kernel
ENDPROC(__primary_switched)

/*
 * end early head section, begin head code that is also used for
 * hotplug and needs to have the same protections as the text region
 */
	.section ".idmap.text","ax"

ENTRY(kimage_vaddr)
	.quad		_text - TEXT_OFFSET

/*
 * If we're fortunate enough to boot at EL2, ensure that the world is
 * sane before dropping to EL1.
 *
 * Returns either BOOT_CPU_MODE_EL1 or BOOT_CPU_MODE_EL2 in w0 if
 * booted in EL1 or EL2 respectively.
 */
ENTRY(el2_setup)
	mrs	x0, CurrentEL
	cmp	x0, #CurrentEL_EL2			// iamroot - 현재 exception level 검사
	b.eq	1f
	mrs	x0, sctlr_el1				// iamroot - 현재 EL1인 경우 여기부터

CPU_BE(	orr	x0, x0, #(3 << 24)	)	// Set the EE and E0E bits for EL1
CPU_LE(	bic	x0, x0, #(3 << 24)	)	// Clear the EE and E0E bits for EL1
									// iamroot - endian 형식에 따라 CPU_BE 또는 CPU_LE 중 하나만 적용됨
									// 
									//	EE : exception endianness(0 : little endian, 1:big endian)
									//		Exception endianness. Indicates the endianness of the translation table data for the translation table lookups. 
									// 		The EE bit is permitted to be cached in a TLB.
									//	EOE : exception level 0(user level)의 endianness - 0 : little endian, 1:big endian
									//		Endianness of explicit data access at EL0.

									// ** Exception level ***
									// EL0 : applications
									// EL1 : kernel level(supervisor)
									// EL2 : hypervisor (run-time에서 OS switching)
									// EL3 : secure monitor

	msr	sctlr_el1, x0
	mov	w0, #BOOT_CPU_MODE_EL1		// This cpu booted in EL1
	isb								// iamroot - isb : instruction syncronization barrier
									//				http://jake.dothome.co.kr/barriers2/ 내용 참조

	ret								// iamroot - 현재 EL1인 경우 여기까지
									// 분석은 여기까지만...;;;

									// iamroot - 여기는 EL2인 경우
1:	mrs	x0, sctlr_el2				// iamroot - EL2인 경우는 PASS!!! 해석할 엄두가 안난다
									// sctlr_el2 : el2의 system control register
									// -> bit 0 : mmu enable
									//    bit 2 : cache enable

CPU_BE(	orr	x0, x0, #(1 << 25)	)	// Set the EE bit for EL2
CPU_LE(	bic	x0, x0, #(1 << 25)	)	// Clear the EE bit for EL2
	msr	sctlr_el2, x0

#ifdef CONFIG_ARM64_VHE				// iamroot - VHE(Virtualization Host Extension)

				/*  iamroot -
					The Virtualization Host (VH) Extensions expand the capabilities of EL2:
					* Designed to improve the support of Type-2 hypervisors
					* Allows the host OS to be run at EL2
					* The host OS requires minimal changes to run at EL2
					* User-space still runs at EL0
					* Guest OSes run at EL1
					* Host has no software running at EL1
					* AArch64 specific
					*EL2 becomes a strict superset of EL1
				*/

	/*
	 * Check for VHE being present. For the rest of the EL2 setup,
	 * x2 being non-zero indicates that we do have VHE, and that the
	 * kernel is intended to run at EL2.
	 */
	mrs	x2, id_aa64mmfr1_el1
	ubfx	x2, x2, #8, #4		/* iamroot - x2의 bit 8부터 4bit 값 추출하여 x2에 저장*/
								/* id_aa64mmfr1_el1의 VH field 추출 */
								/* VH : Indicates whether Virtualization Host Extensions are supported.*/
#else
	mov	x2, xzr
#endif

	/* Hyp configuration. */
	mov	x0, #HCR_RW			// 64-bit EL1
	cbz	x2, set_hcr				// iamroot - x2(VH)의 값이 0이면 바로 set_hcr로 점프 -> HCR_TGE, HCR_E2H는 set하지 않음.
								// HCR - Hypervisor Configuration Register
	orr	x0, x0, #HCR_TGE		// Enable Host Extensions
	orr	x0, x0, #HCR_E2H
set_hcr:
	msr	hcr_el2, x0
	isb						// iamroot
							// https://books.google.co.kr/books?id=5OZblBzjsJ0C&pg=PA100&lpg=PA100&dq=dmb+dsb+isb&source=bl&ots=m38OkiMhGl&sig=03HH-lDhNsFzwswb4uehtkqbHyw&hl=ko&sa=X&ved=0ahUKEwiAtuC004vXAhVKf7wKHebuCKE4FBDoAQg_MAQ#v=onepage&q=dmb%20dsb%20isb&f=false
							// DMB(Data Memory Barrier)
							//	 : Ensures that all memory accesses are completed before new memory access is committed(pipeline상의 memory 단계).
							// DSB(Data Synchronization Barrier)
							//	: Ensures that all memory accesses are completed before the next instruction is executed.
							// ISB(Instruction Synchronization Barrier)
							//	: Flushes the pipeline and ensures that all previous instructions are completed before executing new instructions.

	/*
	 * Allow Non-secure EL1 and EL0 to access physical timer and counter.
	 * This is not necessary for VHE, since the host kernel runs in EL2,
	 * and EL0 accesses are configured in the later stage of boot process.
	 * Note that when HCR_EL2.E2H == 1, CNTHCTL_EL2 has the same bit layout
	 * as CNTKCTL_EL1, and CNTKCTL_EL1 accessing instructions are redefined
	 * to access CNTHCTL_EL2. This allows the kernel designed to run at EL1
	 * to transparently mess with the EL0 bits via CNTKCTL_EL1 access in
	 * EL2.
	 */
	cbnz	x2, 1f			// iamroot : x2가 0이 아닌 경우(VH인 경우) 1f로 점프
	mrs	x0, cnthctl_el2
	orr	x0, x0, #3			// Enable EL1 physical timers
							// iamroot : 여기는 VH가 아닌 경우 - EL1 physical timer가 필요하다
	msr	cnthctl_el2, x0
1:
	msr	cntvoff_el2, xzr		// Clear virtual offset

#ifdef CONFIG_ARM_GIC_V3			// iamroot : GIC(Generic Interrupt Controller)
									// GIC에 대한 내용은 http://jake.dothome.co.kr/ic/ 참고

	/* GICv3 system register access */
	mrs	x0, id_aa64pfr0_el1				// iamroot : 
										// id_aa64pfr0_el1 : Provides additional information about implemented processor features in AArch64.
										// bit[24~27]에 GIC CPU Interface enable/disable
										// 0x0 - disable
										// 0x1 - enable 
	ubfx	x0, x0, #24, #4
	cmp	x0, #1
	b.ne	3f							// GIC CPU enable인 경우 아랫줄 실행

	mrs_s	x0, SYS_ICC_SRE_EL2			// iamroot : SYS_ICC_SRE_EL2는 
										// arch/arm64/include/asm/sysreg.h에 sys_reg(3, 4, 12, 9, 5)로 정의
										// sys_reg 매크로에 의해 encoding된 instruction으로 해당 system register의 내용을 읽어옴
										// ICC_* : GIC physical CPU interface System registers
										// ICC_SRE_EL2 : Interrupt Controller System Register Enable register (EL2)

	orr	x0, x0, #ICC_SRE_EL2_SRE	// Set ICC_SRE_EL2.SRE==1 
									// iamroot - SRE(System Register Enable)
									// 0 : The memory-mapped interface must be used. Access at EL2 to any ICH_* or ICC_*
									// register other than ICC_SRE_EL1 or ICC_SRE_EL2, is trapped to EL2.
									// 1 : The System register interface to the ICH_* registers and the EL1 and EL2 ICC_*
									// registers is enabled for EL2.
									
	orr	x0, x0, #ICC_SRE_EL2_ENABLE	// Set ICC_SRE_EL2.Enable==1
									// iamroot - Enable
									//  Enables lower Exception level access to ICC_SRE_EL1.
									// 0 : Non-secure EL1 accesses to ICC_SRE_EL1 trap to EL2.
									// 1 : Non-secure EL1 accesses to ICC_SRE_EL1 do not trap to EL2.
	
	

	msr_s	SYS_ICC_SRE_EL2, x0
	isb					// Make sure SRE is now set
	mrs_s	x0, SYS_ICC_SRE_EL2		// Read SRE back,

	tbz	x0, #0, 3f			// and check that it sticks 
										// iamroot - x0의 bit[0]이 0이면 3: 으로 jump

	msr_s	SYS_ICH_HCR_EL2, xzr		// Reset ICC_HCR_EL2 to defaults

										// iamroot - ICH_HCR_EL2, Interrupt Controller Hyp Control Register
										// Controls the environment for VMs


3:
#endif

	/* Populate ID registers. */
	mrs	x0, midr_el1			// iamroot - main ID register EL1
								// Provides identification information for the processor, 
								// including an implementer code for the device and a device ID number.

	mrs	x1, mpidr_el1			// iamroot - Multiprocessor Affinity Register, EL1
								// Provides an additional core identification mechanism for scheduling purposes in a cluster system. 
								// EDDEVAFF0 is a read-only copy of MPIDR_EL1[31:0] accessible from the external debug interface.
								// http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.den0024a/CFHCIDCH.html								

	msr	vpidr_el2, x0
	msr	vmpidr_el2, x1

#ifdef CONFIG_COMPAT			// iamroot
								// CONFIG_ COMPAT : "Kernel support for 32-bit EL0"

	msr	hstr_el2, xzr			// Disable CP15 traps to EL2
#endif

	/* EL2 debug */
	mrs	x1, id_aa64dfr0_el1		// Check ID_AA64DFR0_EL1 PMUVer
								// iamroot - id_aa64dfr0_el1
								// Provides top-level information about the debug system in AArch64.
	sbfx	x0, x1, #8, #4		// PMUVer, [11:8]
								// Performance Monitors Extension version.
								// 0x4: Performance monitor system registers implemented, PMUv3.

	cmp	x0, #1
	b.lt	4f				// Skip if no PMU present
	mrs	x0, pmcr_el0			// Disable debug access traps
	ubfx	x0, x0, #11, #5			// to EL2 and allow access to
4:
	csel	x3, xzr, x0, lt			// all PMU counters from EL1

									// iamroot - csel의 정확한 의미는???
									// The conditional select instructions select between the first or second
									// source register, depending on the current state of the condition flag.
									// if no PMU -> x3 = 0
									// else -> x3 = pmcr_el0[15:11](Number of event counters)
								 

	/* Statistical profiling */
	ubfx	x0, x1, #32, #4			// Check ID_AA64DFR0_EL1 PMSVer
									// iamroot - PMSVer : hidden :(

	cbz	x0, 6f				// Skip if SPE not present
							// iamroot - SPE(Statistical Profiling Extension - ARM V8.2 지원)

	cbnz	x2, 5f				// VHE?
	mov	x1, #(MDCR_EL2_E2PB_MASK << MDCR_EL2_E2PB_SHIFT)
							// iamroot - MDCR_EL2(Monitor Debugger Configuration Register for EL2)
							// 뭔지 모르겠다;;; manual상에는 reserved로 나옴


	orr	x3, x3, x1			// If we don't have VHE, then
	b	6f				// use EL1&0 translation.
5:						// For VHE, use EL2 translation
	orr	x3, x3, #MDCR_EL2_TPMS		// and disable access from EL1
6:
	msr	mdcr_el2, x3			// Configure debug traps

	/* Stage-2 translation */
	msr	vttbr_el2, xzr			// iamroot - Virtualization Translation Table Base Register, EL2
								// 이걸 0으로 set

	cbz	x2, install_el2_stub

	mov	w0, #BOOT_CPU_MODE_EL2		// This CPU booted in EL2
	isb
	ret

install_el2_stub:
	/*
	 * When VHE is not in use, early init of EL2 and EL1 needs to be
	 * done here.
	 * When VHE _is_ in use, EL1 will not be used in the host and
	 * requires no configuration, and all non-hyp-specific EL2 setup
	 * will be done via the _EL1 system register aliases in __cpu_setup.
	 */
	/* sctlr_el1 */
	mov	x0, #0x0800			// Set/clear RES{1,0} bits
CPU_BE(	movk	x0, #0x33d0, lsl #16	)	// Set EE and E0E on BE systems
CPU_LE(	movk	x0, #0x30d0, lsl #16	)	// Clear EE and E0E on LE systems
							// iamroot - endianess에 따라 sctlr_el1(System Control Register, EL1)의 EE, EOE를 set 또는 clear   
							// EE, [25]
							// Exception endianness. The value of this bit controls the endianness for explicit data accesses at EL1. 
							// This value also indicates the endianness of the translation table data for translation table lookups. 
							// The possible values of this bit are:
							// 0	Little-endian.
							// 1	Big-endian.
							// The reset value of this bit is determined by the CFGEND configuration pin.



	msr	sctlr_el1, x0

	/* Coprocessor traps. */
	mov	x0, #0x33ff
	msr	cptr_el2, x0			// Disable copro. traps to EL2
								// iamroot - EL2로의 trap을 disable한다

	/* Hypervisor stub */
	adr_l	x0, __hyp_stub_vectors
	msr	vbar_el2, x0			// iamroot - vbar_el2(Vector Base Address Register, EL2)
								// 여기에 hyp stub vectors의 주소를 대입 - hyp에서 exception 발생 시, 이걸 통해 jump
								// 실제로 trap은 발생 안할듯 	

	/* spsr */
	mov	x0, #(PSR_F_BIT | PSR_I_BIT | PSR_A_BIT | PSR_D_BIT |\
		      PSR_MODE_EL1h)
	msr	spsr_el2, x0
	msr	elr_el2, lr
	mov	w0, #BOOT_CPU_MODE_EL2		// This CPU booted in EL2
	eret
ENDPROC(el2_setup)

/*
 * Sets the __boot_cpu_mode flag depending on the CPU boot mode passed
 * in w0. See arch/arm64/include/asm/virt.h for more info.
 */
set_cpu_boot_mode_flag:
	adr_l	x1, __boot_cpu_mode
	cmp	w0, #BOOT_CPU_MODE_EL2
	b.ne	1f
	add	x1, x1, #4
1:	str	w0, [x1]			// This CPU has booted in EL1
	dmb	sy
	dc	ivac, x1			// Invalidate potentially stale cache line
	ret
ENDPROC(set_cpu_boot_mode_flag)

/*
 * These values are written with the MMU off, but read with the MMU on.
 * Writers will invalidate the corresponding address, discarding up to a
 * 'Cache Writeback Granule' (CWG) worth of data. The linker script ensures
 * sufficient alignment that the CWG doesn't overlap another section.
 */
	.pushsection ".mmuoff.data.write", "aw"
/*
 * We need to find out the CPU boot mode long after boot, so we need to
 * store it in a writable variable.
 *
 * This is not in .bss, because we set it sufficiently early that the boot-time
 * zeroing of .bss would clobber it.
 */
ENTRY(__boot_cpu_mode)
	.long	BOOT_CPU_MODE_EL2
	.long	BOOT_CPU_MODE_EL1
/*
 * The booting CPU updates the failed status @__early_cpu_boot_status,
 * with MMU turned off.
 */
ENTRY(__early_cpu_boot_status)
	.long 	0

	.popsection

	/*
	 * This provides a "holding pen" for platforms to hold all secondary
	 * cores are held until we're ready for them to initialise.
	 */
ENTRY(secondary_holding_pen)
	bl	el2_setup			// Drop to EL1, w0=cpu_boot_mode
	bl	set_cpu_boot_mode_flag
	mrs	x0, mpidr_el1
	mov_q	x1, MPIDR_HWID_BITMASK
	and	x0, x0, x1
	adr_l	x3, secondary_holding_pen_release
pen:	ldr	x4, [x3]
	cmp	x4, x0
	b.eq	secondary_startup
	wfe
	b	pen
ENDPROC(secondary_holding_pen)

	/*
	 * Secondary entry point that jumps straight into the kernel. Only to
	 * be used where CPUs are brought online dynamically by the kernel.
	 */
ENTRY(secondary_entry)
	bl	el2_setup			// Drop to EL1
	bl	set_cpu_boot_mode_flag
	b	secondary_startup
ENDPROC(secondary_entry)

secondary_startup:
	/*
	 * Common entry point for secondary CPUs.
	 */
	bl	__cpu_setup			// initialise processor
	bl	__enable_mmu
	ldr	x8, =__secondary_switched
	br	x8
ENDPROC(secondary_startup)

__secondary_switched:
	adr_l	x5, vectors
	msr	vbar_el1, x5
	isb

	adr_l	x0, secondary_data
	ldr	x1, [x0, #CPU_BOOT_STACK]	// get secondary_data.stack
	mov	sp, x1
	ldr	x2, [x0, #CPU_BOOT_TASK]
	msr	sp_el0, x2
	mov	x29, #0
	b	secondary_start_kernel
ENDPROC(__secondary_switched)

/*
 * The booting CPU updates the failed status @__early_cpu_boot_status,
 * with MMU turned off.
 *
 * update_early_cpu_boot_status tmp, status
 *  - Corrupts tmp1, tmp2
 *  - Writes 'status' to __early_cpu_boot_status and makes sure
 *    it is committed to memory.
 */

	.macro	update_early_cpu_boot_status status, tmp1, tmp2
	mov	\tmp2, #\status
	adr_l	\tmp1, __early_cpu_boot_status
	str	\tmp2, [\tmp1]
	dmb	sy
	dc	ivac, \tmp1			// Invalidate potentially stale cache line
	.endm

/*
 * Enable the MMU.
 *
 *  x0  = SCTLR_EL1 value for turning on the MMU.
 *
 * Returns to the caller via x30/lr. This requires the caller to be covered
 * by the .idmap.text section.
 *
 * Checks if the selected granule size is supported by the CPU.
 * If it isn't, park the CPU
 */
ENTRY(__enable_mmu)
	mrs	x1, ID_AA64MMFR0_EL1
	ubfx	x2, x1, #ID_AA64MMFR0_TGRAN_SHIFT, 4
	cmp	x2, #ID_AA64MMFR0_TGRAN_SUPPORTED
	b.ne	__no_granule_support
	update_early_cpu_boot_status 0, x1, x2
	adrp	x1, idmap_pg_dir
	adrp	x2, swapper_pg_dir
	msr	ttbr0_el1, x1			// load TTBR0
	msr	ttbr1_el1, x2			// load TTBR1
	isb
	msr	sctlr_el1, x0
	isb
	/*
	 * Invalidate the local I-cache so that any instructions fetched
	 * speculatively from the PoC are discarded, since they may have
	 * been dynamically patched at the PoU.
	 */
	ic	iallu
	dsb	nsh
	isb
	ret
ENDPROC(__enable_mmu)

__no_granule_support:
	/* Indicate that this CPU can't boot and is stuck in the kernel */
	update_early_cpu_boot_status CPU_STUCK_IN_KERNEL, x1, x2
1:
	wfe
	wfi
	b	1b
ENDPROC(__no_granule_support)

#ifdef CONFIG_RELOCATABLE
__relocate_kernel:
	/*
	 * Iterate over each entry in the relocation table, and apply the
	 * relocations in place.
	 */
	ldr	w9, =__rela_offset		// offset to reloc table
	ldr	w10, =__rela_size		// size of reloc table

	mov_q	x11, KIMAGE_VADDR		// default virtual offset
	add	x11, x11, x23			// actual virtual offset
	add	x9, x9, x11			// __va(.rela)
	add	x10, x9, x10			// __va(.rela) + sizeof(.rela)

0:	cmp	x9, x10
	b.hs	1f
	ldp	x11, x12, [x9], #24
	ldr	x13, [x9, #-8]
	cmp	w12, #R_AARCH64_RELATIVE
	b.ne	0b
	add	x13, x13, x23			// relocate
	str	x13, [x11, x23]
	b	0b
1:	ret
ENDPROC(__relocate_kernel)
#endif

__primary_switch:
#ifdef CONFIG_RANDOMIZE_BASE
	mov	x19, x0				// preserve new SCTLR_EL1 value
	mrs	x20, sctlr_el1			// preserve old SCTLR_EL1 value
#endif

	bl	__enable_mmu
#ifdef CONFIG_RELOCATABLE
	bl	__relocate_kernel
#ifdef CONFIG_RANDOMIZE_BASE
	ldr	x8, =__primary_switched
	adrp	x0, __PHYS_OFFSET
	blr	x8

	/*
	 * If we return here, we have a KASLR displacement in x23 which we need
	 * to take into account by discarding the current kernel mapping and
	 * creating a new one.
	 */
	msr	sctlr_el1, x20			// disable the MMU
	isb
	bl	__create_page_tables		// recreate kernel mapping

	tlbi	vmalle1				// Remove any stale TLB entries
	dsb	nsh

	msr	sctlr_el1, x19			// re-enable the MMU
	isb
	ic	iallu				// flush instructions fetched
	dsb	nsh				// via old mapping
	isb

	bl	__relocate_kernel
#endif
#endif
	ldr	x8, =__primary_switched
	adrp	x0, __PHYS_OFFSET
	br	x8
ENDPROC(__primary_switch)
