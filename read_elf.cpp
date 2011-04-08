/*
* Copyright (C) 2006 Filippos Papadopoulos
*
* Authors:
*  Filippos Papadopoulos
*  Matthias Klumpp
*
* This unit is free software: you can redistribute it and/or modify it under
* the terms of the GNU General Public License as published by the Free Software
* Foundation, version 3.
*
* This unit is distributed in the hope that it will be useful, but WITHOUT
* ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
* FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License v3
* along with this library. If not, see <http://www.gnu.org/licenses/>.
*/

#include "read_elf.h"
#include <QtCore>
#include <QtGui>

using std::vector;
using std::cout;

extern vector<unsigned int> displayError;

QWidget *global_parent;

#if __GNUC__ >= 2
/* Define BFD64 here, even if our default architecture is 32 bit ELF
   as this will allow us to read in and parse 64bit and 32bit ELF files.
   Only do this if we belive that the compiler can support a 64 bit
   data type.  For now we only rely on GCC being able to do this.  */
#define BFD64
#endif


#include <bfd.h>
#include "elf/common.h"
#include "elf/external.h"
#include "elf/internal.h"
#include "elf/dwarf2.h"

/* The following headers use the elf/reloc-macros.h file to
   automatically generate relocation recognition functions
   such as elf_mips_reloc_type()  */

#define RELOC_MACROS_GEN_FUNC

#include "elf/alpha.h"
/*
   #include "elf/arc.h"
   #include "elf/arm.h"
   #include "elf/avr.h"
   #include "elf/cris.h"
   #include "elf/d10v.h"
   #include "elf/d30v.h"
   #include "elf/dlx.h"
   #include "elf/fr30.h"
   #include "elf/frv.h"
   #include "elf/h8.h"
*/
#include "elf/hppa.h"
#include "elf/sparc.h"
#include "elf/i386.h"
#include "elf/ia64.h"
/*
   #include "elf/m68k.h"
   #include "elf/mips.h"
   #include "elf/vax.h"
   #include "elf/i860.h"
   #include "elf/i960.h"
   #include "elf/m32r.h"
   #include "elf/m68hc11.h"
   #include "elf/mcore.h"
   #include "elf/mmix.h"
   #include "elf/mn10200.h"
   #include "elf/mn10300.h"
   #include "elf/or32.h"
   #include "elf/pj.h"
*/
#include "elf/ppc.h"
/*
   #include "elf/s390.h"
   #include "elf/sh.h"
   #include "elf/v850.h"
*/
#include "elf/x86-64.h"
//#include "elf/xstormy16.h"
//#include "bucomm.h"
#include "getopt.h"

char *         program_name = "read_elf";
unsigned int      dynamic_addr;
bfd_size_type     dynamic_size;
unsigned int      rela_addr;
unsigned int      rela_size;
char *         dynamic_strings;
char *         string_table;
unsigned long     string_table_length;
unsigned long           num_dynamic_syms;
Elf_Internal_Sym *   dynamic_symbols;
Elf_Internal_Syminfo *  dynamic_syminfo;
unsigned long     dynamic_syminfo_offset;
unsigned int      dynamic_syminfo_nent;
char        program_interpreter [64];
int         dynamic_info[DT_JMPREL + 1];
int         version_info[16];
int         loadaddr = 0;
Elf_Internal_Ehdr       elf_header;
Elf_Internal_Shdr *     section_headers;
Elf_Internal_Dyn *      dynamic_segment;
Elf_Internal_Shdr *     symtab_shndx_hdr;
int         show_name;
int         do_dynamic = 1;
int         do_syms;
int         do_reloc;
int         do_sections;
int         do_segments;
int         do_unwind;
int         do_using_dynamic;
int         do_header;
int         do_dump;
int         do_version;
int         do_wide;
int         do_histogram;
int         do_debugging;
int                     do_debug_info;
int                     do_debug_abbrevs;
int                     do_debug_lines;
int                     do_debug_pubnames;
int                     do_debug_aranges;
int                     do_debug_frames;
int                     do_debug_frames_interp;
int         do_debug_macinfo;
int         do_debug_str;
int                     do_debug_loc;
int                     do_arch;
int                     do_notes;
int         is_32bit_elf;

/* A dynamic array of flags indicating which sections require dumping.  */
char *         dump_sects = NULL;
unsigned int      num_dump_sects = 0;

#define HEX_DUMP  (1 << 0)
#define DISASS_DUMP  (1 << 1)
#define DEBUG_DUMP   (1 << 2)

/* How to rpint a vma value.  */
typedef enum print_mode
{
    HEX,
    DEC,
    DEC_5,
    UNSIGNED,
    PREFIX_HEX,
    FULL_HEX,
    LONG_HEX
}
print_mode;

/* Forward declarations for dumb compilers.  */
static void      print_vma          PARAMS ((bfd_vma, print_mode));
//static void       print_symbol          PARAMS ((int, char *));
static bfd_vma (*         byte_get)                   PARAMS ((unsigned char *, int));
static bfd_vma            byte_get_little_endian      PARAMS ((unsigned char *, int));
static bfd_vma            byte_get_big_endian         PARAMS ((unsigned char *, int));
//static const char *       get_mips_dynamic_type       PARAMS ((unsigned long));
static const char *       get_sparc64_dynamic_type    PARAMS ((unsigned long));
static const char *       get_ppc64_dynamic_type      PARAMS ((unsigned long));
//static const char *       get_parisc_dynamic_type     PARAMS ((unsigned long));*/
static const char *       get_dynamic_type            PARAMS ((unsigned long));
//static int        slurp_rela_relocs         PARAMS ((FILE *, unsigned long, unsigned long, Elf_Internal_Rela **, unsigned long *));
//static int        slurp_rel_relocs         PARAMS ((FILE *, unsigned long, unsigned long, Elf_Internal_Rel **, unsigned long *));
//static int                dump_relocations            PARAMS ((FILE *, unsigned long, unsigned long, Elf_Internal_Sym *, unsigned long, char *, int));
static char *             get_file_type               PARAMS ((unsigned));
static char *             get_machine_name            PARAMS ((unsigned));
//static void       decode_ARM_machine_flags    PARAMS ((unsigned, char []));
static char *             get_machine_flags           PARAMS ((unsigned, unsigned));
//static const char *       get_mips_segment_type       PARAMS ((unsigned long));
//static const char *       get_parisc_segment_type     PARAMS ((unsigned long));
static const char *       get_ia64_segment_type       PARAMS ((unsigned long));
static const char *       get_segment_type            PARAMS ((unsigned long));
//static const char *       get_mips_section_type_name  PARAMS ((unsigned int));
//static const char *       get_parisc_section_type_name PARAMS ((unsigned int));
static const char *       get_ia64_section_type_name  PARAMS ((unsigned int));
static const char *       get_section_type_name       PARAMS ((unsigned int));
/*static const char *       get_symbol_binding          PARAMS ((unsigned int));
static const char *       get_symbol_type             PARAMS ((unsigned int));
static const char *       get_symbol_visibility       PARAMS ((unsigned int));
static const char *       get_symbol_index_type       PARAMS ((unsigned int));*/
static const char *       get_dynamic_flags         PARAMS ((bfd_vma));
//static void               usage                       PARAMS ((void));
//static void               parse_args                  PARAMS ((int, char **));
static int                process_file_header         PARAMS ((void));
static int                process_program_headers     PARAMS ((FILE *));
static int                process_section_headers     PARAMS ((FILE *));
//static int        process_unwind         PARAMS ((FILE *));
//static void               dynamic_segment_mips_val    PARAMS ((Elf_Internal_Dyn *));
//static void               dynamic_segment_parisc_val  PARAMS ((Elf_Internal_Dyn *));
static int                process_dynamic_segment     PARAMS ((FILE *));
//static int                process_symbol_table        PARAMS ((FILE *));
//static int                process_syminfo             PARAMS ((FILE *));
//static int                process_section_contents    PARAMS ((FILE *));
//static void               process_mips_fpe_exception  PARAMS ((int));
//static int                process_mips_specific       PARAMS ((FILE *));
int                         processFile                PARAMS ((char *, QWidget *parent));
//static int                process_relocs              PARAMS ((FILE *));
static int                process_version_sections    PARAMS ((FILE *));
char *             get_ver_flags               PARAMS ((unsigned int));
static int                get_32bit_section_headers   PARAMS ((FILE *, unsigned int));
static int                get_64bit_section_headers   PARAMS ((FILE *, unsigned int));
static int       get_32bit_program_headers   PARAMS ((FILE *, Elf_Internal_Phdr *));
static int       get_64bit_program_headers   PARAMS ((FILE *, Elf_Internal_Phdr *));
static int                get_file_header             PARAMS ((FILE *));
static Elf_Internal_Sym * get_32bit_elf_symbols       PARAMS ((FILE *, Elf_Internal_Shdr *));
static Elf_Internal_Sym * get_64bit_elf_symbols       PARAMS ((FILE *, Elf_Internal_Shdr *));
static const char *    get_elf_section_flags       PARAMS ((bfd_vma));
//static int *              get_dynamic_data            PARAMS ((FILE *, unsigned int));
static int                get_32bit_dynamic_segment   PARAMS ((FILE *));
static int                get_64bit_dynamic_segment   PARAMS ((FILE *));
#ifdef SUPPORT_DISASSEMBLY
static int             disassemble_section         PARAMS ((Elf32_Internal_Shdr *, FILE *));
#endif
//static int             dump_section                PARAMS ((Elf32_Internal_Shdr *, FILE *));
//static int             display_debug_section       PARAMS ((Elf32_Internal_Shdr *, FILE *));
/*static int                display_debug_info          PARAMS ((Elf32_Internal_Shdr *, unsigned char *, FILE *));
static int                display_debug_not_supported PARAMS ((Elf32_Internal_Shdr *, unsigned char *, FILE *));
static int                prescan_debug_info          PARAMS ((Elf32_Internal_Shdr *, unsigned char *, FILE *));
static int                display_debug_lines         PARAMS ((Elf32_Internal_Shdr *, unsigned char *, FILE *));
static int                display_debug_pubnames      PARAMS ((Elf32_Internal_Shdr *, unsigned char *, FILE *));
static int                display_debug_abbrev        PARAMS ((Elf32_Internal_Shdr *, unsigned char *, FILE *));
static int                display_debug_aranges       PARAMS ((Elf32_Internal_Shdr *, unsigned char *, FILE *));
static int                display_debug_frames        PARAMS ((Elf32_Internal_Shdr *, unsigned char *, FILE *));
static int                display_debug_macinfo       PARAMS ((Elf32_Internal_Shdr *, unsigned char *, FILE *));
static int                display_debug_str           PARAMS ((Elf32_Internal_Shdr *, unsigned char *, FILE *));
static int                display_debug_loc           PARAMS ((Elf32_Internal_Shdr *, unsigned char *, FILE *));
static unsigned char *    process_abbrev_section      PARAMS ((unsigned char *, unsigned char *));
static void               load_debug_str              PARAMS ((FILE *));
static void               free_debug_str              PARAMS ((void));
static const char *       fetch_indirect_string       PARAMS ((unsigned long));
static void               load_debug_loc              PARAMS ((FILE *));
static void               free_debug_loc              PARAMS ((void));
static unsigned long      read_leb128                 PARAMS ((unsigned char *, int *, int));
static int                process_extended_line_op    PARAMS ((unsigned char *, int, int));
static void               reset_state_machine         PARAMS ((int));
static char *             get_TAG_name                PARAMS ((unsigned long));
static char *             get_AT_name                 PARAMS ((unsigned long));
static char *             get_FORM_name               PARAMS ((unsigned long));
static void               free_abbrevs                PARAMS ((void));
static void               add_abbrev                  PARAMS ((unsigned long, unsigned long, int));
static void               add_abbrev_attr             PARAMS ((unsigned long, unsigned long));
static unsigned char *    read_and_display_attr       PARAMS ((unsigned long, unsigned long, unsigned char *, unsigned long, unsigned long));
static unsigned char *    read_and_display_attr_value PARAMS ((unsigned long, unsigned long, unsigned char *, unsigned long, unsigned long));
static unsigned char *    display_block               PARAMS ((unsigned char *, unsigned long));
static void               decode_location_expression  PARAMS ((unsigned char *, unsigned int, unsigned long));*/
//static void       request_dump                PARAMS ((unsigned int, int));
static const char *       get_elf_class               PARAMS ((unsigned int));
static const char *       get_data_encoding           PARAMS ((unsigned int));
static const char *       get_osabi_name              PARAMS ((unsigned int));
/*static int        guess_is_rela               PARAMS ((unsigned long));
static const char *    get_note_type               PARAMS ((unsigned int));
static const char *    get_netbsd_elfcore_note_type   PARAMS ((unsigned int));*/
/*static int        process_note             PARAMS ((Elf32_Internal_Note *));
static int       process_corefile_note_segment  PARAMS ((FILE *, bfd_vma, bfd_vma));
static int       process_corefile_note_segments PARAMS ((FILE *));
static int       process_corefile_contents    PARAMS ((FILE *));
static int       process_arch_specific     PARAMS ((FILE *));
static int       process_gnu_liblist       PARAMS ((FILE *));
*/
typedef int Elf32_Word;

#ifndef TRUE
#define TRUE     1
#define FALSE    0
#endif
#define UNKNOWN -1

#define SECTION_NAME(X)  ((X) == NULL ? "<none>" : \
((X)->sh_name >= string_table_length \
 ? "<corrupt>" : string_table + (X)->sh_name))

/* Given st_shndx I, map to section_headers index.  */
#define SECTION_HEADER_INDEX(I)            \
((I) < SHN_LORESERVE              \
 ? (I)                \
     : ((I) <= SHN_HIRESERVE          \
        ? 0                  \
            : (I) - (SHN_HIRESERVE + 1 - SHN_LORESERVE)))

/* Reverse of the above.  */
#define SECTION_HEADER_NUM(N)          \
((N) < SHN_LORESERVE              \
 ? (N)                \
     : (N) + (SHN_HIRESERVE + 1 - SHN_LORESERVE))

#define SECTION_HEADER(I) (section_headers + SECTION_HEADER_INDEX (I))

#define DT_VERSIONTAGIDX(tag) (DT_VERNEEDNUM - (tag))  /* Reverse order! */

#define BYTE_GET(field)  byte_get (field, sizeof (field))

/* If we can support a 64 bit data type then BFD64 should be defined
   and sizeof (bfd_vma) == 8.  In this case when translating from an
   external 8 byte field to an internal field, we can assume that the
   internal field is also 8 bytes wide and so we can extract all the data.
   If, however, BFD64 is not defined, then we must assume that the
   internal data structure only has 4 byte wide fields that are the
   equivalent of the 8 byte wide external counterparts, and so we must
   truncate the data.  */
#ifdef  BFD64
#define BYTE_GET8(field)   byte_get (field, -8)
#else
#define BYTE_GET8(field)   byte_get (field, 8)
#endif

#define NUM_ELEM(array)    (sizeof (array) / sizeof ((array)[0]))

#define GET_ELF_SYMBOLS(file, section)       \
(is_32bit_elf ? get_32bit_elf_symbols (file, section)  \
    : get_64bit_elf_symbols (file, section))




/* for readelf */

static PTR get_data PARAMS ((PTR, FILE *, long, size_t, const char *));

/* Print a VMA value.  */
static void print_vma(bfd_vma vma, print_mode mode)
{
#ifdef BFD64
    if (is_32bit_elf)
#endif
    {
        switch (mode)
        {
        case FULL_HEX: printf ("0x"); /* drop through */
        case LONG_HEX: printf ("%8.8lx", (unsigned long) vma); break;
        case PREFIX_HEX: printf ("0x"); /* drop through */
        case HEX: printf ("%lx", (unsigned long) vma); break;
        case DEC: printf ("%ld", (unsigned long) vma); break;
        case DEC_5: printf ("%5ld", (long) vma); break;
        case UNSIGNED: printf ("%lu", (unsigned long) vma); break;
        }
    }
#ifdef BFD64
    else
    {
        switch (mode)
        {
        case FULL_HEX:
            printf ("0x");
            /* drop through */

        case LONG_HEX:
            printf_vma (vma);
            break;

        case PREFIX_HEX:
            printf ("0x");
            /* drop through */

        case HEX:
#if BFD_HOST_64BIT_LONG
            printf ("%lx", vma);
#else
            if (_bfd_int64_high (vma))
                printf ("%lx%8.8lx", _bfd_int64_high (vma), _bfd_int64_low (vma));
            else
                printf ("%lx", _bfd_int64_low (vma));
#endif
            break;

        case DEC:
#if BFD_HOST_64BIT_LONG
            printf ("%ld", vma);
#else
            if (_bfd_int64_high (vma))
                /* ugg */
                printf ("++%ld", _bfd_int64_low (vma));
            else
                printf ("%ld", _bfd_int64_low (vma));
#endif
            break;

        case DEC_5:
#if BFD_HOST_64BIT_LONG
            printf ("%5ld", vma);
#else
            if (_bfd_int64_high (vma))
                /* ugg */
                printf ("++%ld", _bfd_int64_low (vma));
            else
                printf ("%5ld", _bfd_int64_low (vma));
#endif
            break;

        case UNSIGNED:
#if BFD_HOST_64BIT_LONG
            printf ("%lu", vma);
#else
            if (_bfd_int64_high (vma))
                /* ugg */
                printf ("++%lu", _bfd_int64_low (vma));
            else
                printf ("%lu", _bfd_int64_low (vma));
#endif
            break;
        }
    }
#endif
}

int processFile(char * file_name, QWidget *parent)
{
    FILE *       file;
    struct stat  statbuf;
    unsigned int i;

    global_parent = parent;

    if (stat (file_name, & statbuf) < 0)
    {
        /*  QMessageBox::warning(parent, "readELF warning", QString("Cannot stat input file %1").arg(file_name),
                           QMessageBox::Ok, QMessageBox::NoButton);*/
        //      displayError = true;
        fprintf(stderr, "Cannot stat input file %s\n", file_name);
        return 1;
    }

    file = fopen (file_name, "rb");
    if (file == NULL)
    {
        QMessageBox::warning(parent, "readELF warning", QString("Input file %1 not found").arg(file_name),
                             QMessageBox::Ok, QMessageBox::NoButton);
        //printf("Input file %s not found.\n", file_name);
        return 1;
    }

    if (! get_file_header (file))
    {
        QMessageBox::warning(parent, "readELF warning", QString("%1: Failed to read file header").arg(file_name),
                             QMessageBox::Ok, QMessageBox::NoButton);
        //      printf("%s: Failed to read file header\n", file_name);
        fclose (file);
        return 1;
    }

    /* Initialise per file variables.  */
    for (i = NUM_ELEM (version_info); i--;)
        version_info[i] = 0;

    for (i = NUM_ELEM (dynamic_info); i--;)
        dynamic_info[i] = 0;

    /* Process the file.  */
    if (show_name)
        ;//printf("\nFile: %s\n", file_name);

    if (! process_file_header ())
    {
        fclose (file);
        return 1;
    }

    process_section_headers (file);

    process_program_headers (file);

    process_dynamic_segment (file);

    /*  process_relocs (file);

  process_unwind (file);

  process_symbol_table (file);

  process_syminfo (file);

  process_version_sections (file);

  process_section_contents (file);

  process_corefile_contents (file);

  process_gnu_liblist (file);

  process_arch_specific (file);
*/
    fclose (file);

    if (section_headers)
    {
        free (section_headers);
        section_headers = NULL;
    }

    if (string_table)
    {
        free (string_table);
        string_table = NULL;
        string_table_length = 0;
    }

    if (dynamic_strings)
    {
        free (dynamic_strings);
        dynamic_strings = NULL;
    }

    if (dynamic_symbols)
    {
        free (dynamic_symbols);
        dynamic_symbols = NULL;
        num_dynamic_syms = 0;
    }

    if (dynamic_syminfo)
    {
        free (dynamic_syminfo);
        dynamic_syminfo = NULL;
    }

    return 0;
}



static const char *get_section_type_name (unsigned int sh_type)
{
    static char buff [32];

    switch (sh_type)
    {
    case SHT_NULL:      return "NULL";
    case SHT_PROGBITS:     return "PROGBITS";
    case SHT_SYMTAB:    return "SYMTAB";
    case SHT_STRTAB:    return "STRTAB";
    case SHT_RELA:      return "RELA";
    case SHT_HASH:      return "HASH";
    case SHT_DYNAMIC:      return "DYNAMIC";
    case SHT_NOTE:      return "NOTE";
    case SHT_NOBITS:    return "NOBITS";
    case SHT_REL:    return "REL";
    case SHT_SHLIB:     return "SHLIB";
    case SHT_DYNSYM:    return "DYNSYM";
    case SHT_INIT_ARRAY:   return "INIT_ARRAY";
    case SHT_FINI_ARRAY:   return "FINI_ARRAY";
    case SHT_PREINIT_ARRAY:   return "PREINIT_ARRAY";
    case SHT_GROUP:     return "GROUP";
    case SHT_SYMTAB_SHNDX: return "SYMTAB SECTION INDICIES";
    case SHT_GNU_verdef:   return "VERDEF";
    case SHT_GNU_verneed:   return "VERNEED";
    case SHT_GNU_versym:   return "VERSYM";
    case 0x6ffffff0:          return "VERSYM";
    case 0x6ffffffc:          return "VERDEF";
    case 0x7ffffffd:    return "AUXILIARY";
    case 0x7fffffff:    return "FILTER";
    case SHT_GNU_LIBLIST:   return "GNU_LIBLIST";

    default:
        if ((sh_type >= SHT_LOPROC) && (sh_type <= SHT_HIPROC))
        {
            const char * result = NULL;

            switch (elf_header.e_machine)
            {
            case EM_MIPS:
            case EM_MIPS_RS3_LE:
                //result = get_mips_section_type_name (sh_type);
                break;
            case EM_PARISC:
                // result = get_parisc_section_type_name (sh_type);
                break;
            case EM_IA_64:
                result = get_ia64_section_type_name (sh_type);
                break;
            default:
                result = NULL;
                break;
            }

            if (result != NULL)
                return result;

            sprintf (buff, "LOPROC+%x", sh_type - SHT_LOPROC);
        }
        else if ((sh_type >= SHT_LOOS) && (sh_type <= SHT_HIOS))
            sprintf (buff, "LOOS+%x", sh_type - SHT_LOOS);
        else if ((sh_type >= SHT_LOUSER) && (sh_type <= SHT_HIUSER))
            sprintf (buff, "LOUSER+%x", sh_type - SHT_LOUSER);
        else
            sprintf (buff, "<unknown>: %x", sh_type);

        return buff;
    }
}


static const char *get_ia64_section_type_name(unsigned int sh_type)
{
    switch (sh_type)
    {
    case SHT_IA_64_EXT:     return "IA_64_EXT";
    case SHT_IA_64_UNWIND: return "IA_64_UNWIND";
    default:
        break;
    }
    return NULL;
}



static const char *get_elf_section_flags (  bfd_vma sh_flags)
{
    static char buff [32];

    * buff = 0;

    while (sh_flags)
    {
        bfd_vma flag;

        flag = sh_flags & - sh_flags;
        sh_flags &= ~ flag;

        switch (flag)
        {
        case SHF_WRITE:            strcat (buff, "W"); break;
        case SHF_ALLOC:            strcat (buff, "A"); break;
        case SHF_EXECINSTR:        strcat (buff, "X"); break;
        case SHF_MERGE:            strcat (buff, "M"); break;
        case SHF_STRINGS:          strcat (buff, "S"); break;
        case SHF_INFO_LINK:        strcat (buff, "I"); break;
        case SHF_LINK_ORDER:       strcat (buff, "L"); break;
        case SHF_OS_NONCONFORMING: strcat (buff, "O"); break;
        case SHF_GROUP:            strcat (buff, "G"); break;
        case SHF_TLS:        strcat (buff, "T"); break;

        default:
            if (flag & SHF_MASKOS)
            {
                strcat (buff, "o");
                sh_flags &= ~ SHF_MASKOS;
            }
            else if (flag & SHF_MASKPROC)
            {
                strcat (buff, "p");
                sh_flags &= ~ SHF_MASKPROC;
            }
            else
                strcat (buff, "x");
            break;
        }
    }

    return buff;
}



static int process_section_headers (FILE * file)
{
    Elf_Internal_Shdr * section;
    unsigned int        i;

    section_headers = NULL;

    if (elf_header.e_shnum == 0)
    {
        if (do_sections)
            QMessageBox::warning(global_parent, "readELF warning", QString("There are no sections in this file"),
                                 QMessageBox::Ok, QMessageBox::NoButton);

        //printf ("\nThere are no sections in this file.\n");

        return 1;
    }

    if (do_sections && !do_header)
        ;//printf ("There are %d section headers, starting at offset 0x%lx:\n",
    //elf_header.e_shnum, (unsigned long) elf_header.e_shoff);

    if (is_32bit_elf)
    {
        if (! get_32bit_section_headers (file, elf_header.e_shnum))
            return 0;
    }
    else if (! get_64bit_section_headers (file, elf_header.e_shnum))
        return 0;

    /* Read in the string table, so that we have names to display.  */
    section = SECTION_HEADER (elf_header.e_shstrndx);

    if (section->sh_size != 0)
    {
        string_table = (char *) get_data (NULL, file, section->sh_offset,
                                          section->sh_size, "string table");

        string_table_length = section->sh_size;
    }

    /* Scan the sections for the dynamic symbol table
     and dynamic string table and debug sections.  */
    dynamic_symbols = NULL;
    dynamic_strings = NULL;
    dynamic_syminfo = NULL;

    for (i = 0, section = section_headers;
         i < elf_header.e_shnum;
         i ++, section ++)
    {
        const char *name = SECTION_NAME (section);

        if (section->sh_type == SHT_DYNSYM)
        {
            if (dynamic_symbols != NULL)
            {
                //printf ("File contains multiple dynamic symbol tables\n");
                continue;
            }

            num_dynamic_syms = section->sh_size / section->sh_entsize;
            dynamic_symbols = GET_ELF_SYMBOLS (file, section);
        }
        else if (section->sh_type == SHT_STRTAB
                 && strcmp (name, ".dynstr") == 0)
        {
            if (dynamic_strings != NULL)
            {
                //printf("File contains multiple dynamic string tables\n");
                continue;
            }

            dynamic_strings = (char *) get_data (NULL, file, section->sh_offset,
                                                 section->sh_size,
                                                 "dynamic strings");
        }
        else if (section->sh_type == SHT_SYMTAB_SHNDX)
        {
            if (symtab_shndx_hdr != NULL)
            {
                //printf("File contains multiple symtab shndx tables\n");
                continue;
            }
            symtab_shndx_hdr = section;
        }
        else if ((do_debugging || do_debug_info || do_debug_abbrevs
                  || do_debug_lines || do_debug_pubnames || do_debug_aranges
                  || do_debug_frames || do_debug_macinfo || do_debug_str
                  || do_debug_loc)
            && strncmp (name, ".debug_", 7) == 0)
            {
            name += 7;

            if (do_debugging
                || (do_debug_info     && (strcmp (name, "info") == 0))
                || (do_debug_abbrevs  && (strcmp (name, "abbrev") == 0))
                || (do_debug_lines    && (strcmp (name, "line") == 0))
                || (do_debug_pubnames && (strcmp (name, "pubnames") == 0))
                || (do_debug_aranges  && (strcmp (name, "aranges") == 0))
                || (do_debug_frames   && (strcmp (name, "frame") == 0))
                || (do_debug_macinfo  && (strcmp (name, "macinfo") == 0))
                || (do_debug_str      && (strcmp (name, "str") == 0))
                || (do_debug_loc      && (strcmp (name, "loc") == 0))
                )
                ;//request_dump (i, DEBUG_DUMP);
        }
        /* linkonce section to be combined with .debug_info at link time.  */
        else if ((do_debugging || do_debug_info)
            && strncmp (name, ".gnu.linkonce.wi.", 17) == 0)
            ;//request_dump (i, DEBUG_DUMP);
        else if (do_debug_frames && strcmp (name, ".eh_frame") == 0)
            ;//request_dump (i, DEBUG_DUMP);
    }

    if (! do_sections)
        return 1;

    if (elf_header.e_shnum > 1)
        ;//printf ("\nSection Headers:\n");
    else
        ;//printf ("\nSection Header:\n");

    /*  if (is_32bit_elf)
    ;//printf  ("  [Nr] Name              Type            Addr     Off    Size   ES Flg Lk Inf Al\n");
  else if (do_wide)
    printf ("  [Nr] Name              Type            Address          Off    Size   ES Flg Lk Inf Al\n");
  else
    {
      printf ("  [Nr] Name              Type             Address           Offset\n");
      printf ("       Size              EntSize          Flags  Link  Info  Align\n");
    }
*/
    for (i = 0, section = section_headers;
         i < elf_header.e_shnum;
         i ++, section ++)
    {
        printf ("  [%2u] %-17.17s %-15.15s ",
                SECTION_HEADER_NUM (i),
                SECTION_NAME (section),
                get_section_type_name (section->sh_type));

        if (is_32bit_elf)
        {
            print_vma (section->sh_addr, LONG_HEX);

            printf ( " %6.6lx %6.6lx %2.2lx",
                     (unsigned long) section->sh_offset,
                     (unsigned long) section->sh_size,
                     (unsigned long) section->sh_entsize);

            printf (" %3s ", get_elf_section_flags (section->sh_flags));

            printf ("%2ld %3lx %2ld\n",
                    (unsigned long) section->sh_link,
                    (unsigned long) section->sh_info,
                    (unsigned long) section->sh_addralign);
        }
        else if (do_wide)
        {
            print_vma (section->sh_addr, LONG_HEX);

            if ((long) section->sh_offset == section->sh_offset)
                printf (" %6.6lx", (unsigned long) section->sh_offset);
            else
            {
                putchar (' ');
                print_vma (section->sh_offset, LONG_HEX);
            }

            if ((unsigned long) section->sh_size == section->sh_size)
                printf (" %6.6lx", (unsigned long) section->sh_size);
            else
            {
                putchar (' ');
                print_vma (section->sh_size, LONG_HEX);
            }

            if ((unsigned long) section->sh_entsize == section->sh_entsize)
                printf (" %2.2lx", (unsigned long) section->sh_entsize);
            else
            {
                putchar (' ');
                print_vma (section->sh_entsize, LONG_HEX);
            }

            printf (" %3s ", get_elf_section_flags (section->sh_flags));

            printf ("%2ld %3lx ",
                    (unsigned long) section->sh_link,
                    (unsigned long) section->sh_info);

            if ((unsigned long) section->sh_addralign == section->sh_addralign)
                printf ("%2ld\n", (unsigned long) section->sh_addralign);
            else
            {
                print_vma (section->sh_addralign, DEC);
                //putchar ('\n');
            }
        }
        else
        {
            putchar (' ');
            print_vma (section->sh_addr, LONG_HEX);
            if ((long) section->sh_offset == section->sh_offset)
                printf ("  %8.8lx", (unsigned long) section->sh_offset);
            else
            {
                //printf ("  ");
                print_vma (section->sh_offset, LONG_HEX);
            }
            //printf ("\n       ");
            print_vma (section->sh_size, LONG_HEX);
            //printf ("  ");
            print_vma (section->sh_entsize, LONG_HEX);

            printf (" %3s ", get_elf_section_flags (section->sh_flags));

            printf ("     %2ld   %3lx     %ld\n",
                    (unsigned long) section->sh_link,
                    (unsigned long) section->sh_info,
                    (unsigned long) section->sh_addralign);
        }
    }

    printf ("Key to Flags:\n\
            W (write), A (alloc), X (execute), M (merge), S (strings)\n\
            I (info), L (link order), G (group), x (unknown)\n\
            O (extra OS processing required) o (OS specific), p (processor specific)\n");

    return 1;
}




char *get_ver_flags(unsigned int flags)
{
    static char buff [32];

    buff[0] = 0;

    if (flags == 0)
        return "none";

    if (flags & VER_FLG_BASE)
        strcat (buff, "BASE ");

    if (flags & VER_FLG_WEAK)
    {
        if (flags & VER_FLG_BASE)
            strcat (buff, "| ");

        strcat (buff, "WEAK ");
    }

    if (flags & ~(VER_FLG_BASE | VER_FLG_WEAK))
        strcat (buff, "| <unknown>");

    return buff;
}


/* Display the contents of the version sections.  */
static int process_version_sections(FILE * file)
{
    Elf32_Internal_Shdr * section;
    unsigned   i;
    int        found = 0;

    if (! do_version)
        return 1;

    for (i = 0, section = section_headers;
         i < elf_header.e_shnum;
         i++, section ++)
    {
        switch (section->sh_type)
        {
        case SHT_GNU_verdef:
            {
                Elf_External_Verdef * edefs;
                unsigned int          idx;
                unsigned int          cnt;

                found = 1;

                printf("\nVersion definition section '%s' contains %ld entries:\n",
                       SECTION_NAME (section), section->sh_info);

                printf("  Addr: 0x");
                printf_vma (section->sh_addr);
                printf ("  Offset: %#08lx  Link: %lx (%s)\n",
                        (unsigned long) section->sh_offset, section->sh_link,
                        SECTION_NAME (SECTION_HEADER (section->sh_link)));

                edefs = ((Elf_External_Verdef *)
                         get_data (NULL, file, section->sh_offset,
                                   section->sh_size,
                                   "version definition section"));
                if (!edefs)
                    break;

                for (idx = cnt = 0; cnt < section->sh_info; ++ cnt)
                {
                    char *                 vstart;
                    Elf_External_Verdef *  edef;
                    Elf_Internal_Verdef    ent;
                    Elf_External_Verdaux * eaux;
                    Elf_Internal_Verdaux   aux;
                    int                    j;
                    int                    isum;

                    vstart = ((char *) edefs) + idx;

                    edef = (Elf_External_Verdef *) vstart;

                    ent.vd_version = BYTE_GET (edef->vd_version);
                    ent.vd_flags   = BYTE_GET (edef->vd_flags);
                    ent.vd_ndx     = BYTE_GET (edef->vd_ndx);
                    ent.vd_cnt     = BYTE_GET (edef->vd_cnt);
                    ent.vd_hash    = BYTE_GET (edef->vd_hash);
                    ent.vd_aux     = BYTE_GET (edef->vd_aux);
                    ent.vd_next    = BYTE_GET (edef->vd_next);

                    printf ("  %#06x: Rev: %d  Flags: %s",
                            idx, ent.vd_version, get_ver_flags (ent.vd_flags));

                    printf ("  Index: %d  Cnt: %d  ",
                            ent.vd_ndx, ent.vd_cnt);

                    vstart += ent.vd_aux;

                    eaux = (Elf_External_Verdaux *) vstart;

                    aux.vda_name = BYTE_GET (eaux->vda_name);
                    aux.vda_next = BYTE_GET (eaux->vda_next);

                    if (dynamic_strings)
                        printf("Name: %s\n", dynamic_strings + aux.vda_name);
                    else
                        printf ("Name index: %ld\n", aux.vda_name);

                    isum = idx + ent.vd_aux;

                    for (j = 1; j < ent.vd_cnt; j ++)
                    {
                        isum   += aux.vda_next;
                        vstart += aux.vda_next;

                        eaux = (Elf_External_Verdaux *) vstart;

                        aux.vda_name = BYTE_GET (eaux->vda_name);
                        aux.vda_next = BYTE_GET (eaux->vda_next);

                        if (dynamic_strings)
                            printf ("  %#06x: Parent %d: %s\n",
                                    isum, j, dynamic_strings + aux.vda_name);
                        else
                            printf ("  %#06x: Parent %d, name index: %ld\n",
                                    isum, j, aux.vda_name);
                    }

                    idx += ent.vd_next;
                }

                free (edefs);
            }
            break;

        case SHT_GNU_verneed:
            {
                Elf_External_Verneed *  eneed;
                unsigned int            idx;
                unsigned int            cnt;

                found = 1;

                printf ("\nVersion needs section '%s' contains %ld entries:\n",
                        SECTION_NAME (section), section->sh_info);

                //printf (" Addr: 0x");
                printf_vma (section->sh_addr);
                printf ("  Offset: %#08lx  Link to section: %ld (%s)\n",
                        (unsigned long) section->sh_offset, section->sh_link,
                        SECTION_NAME (SECTION_HEADER (section->sh_link)));

                eneed = ((Elf_External_Verneed *)
                         get_data (NULL, file, section->sh_offset,
                                   section->sh_size, "version need section"));
                if (!eneed)
                    break;

                for (idx = cnt = 0; cnt < section->sh_info; ++cnt)
                {
                    Elf_External_Verneed * entry;
                    Elf_Internal_Verneed     ent;
                    int                      j;
                    int                      isum;
                    char *                   vstart;

                    vstart = ((char *) eneed) + idx;

                    entry = (Elf_External_Verneed *) vstart;

                    ent.vn_version = BYTE_GET (entry->vn_version);
                    ent.vn_cnt     = BYTE_GET (entry->vn_cnt);
                    ent.vn_file    = BYTE_GET (entry->vn_file);
                    ent.vn_aux     = BYTE_GET (entry->vn_aux);
                    ent.vn_next    = BYTE_GET (entry->vn_next);

                    printf ("  %#06x: Version: %d", idx, ent.vn_version);

                    if (dynamic_strings)
                        printf ("  File: %s", dynamic_strings + ent.vn_file);
                    else
                        printf ("  File: %lx", ent.vn_file);

                    printf ("  Cnt: %d\n", ent.vn_cnt);

                    vstart += ent.vn_aux;

                    for (j = 0, isum = idx + ent.vn_aux; j < ent.vn_cnt; ++j)
                    {
                        Elf_External_Vernaux * eaux;
                        Elf_Internal_Vernaux   aux;

                        eaux = (Elf_External_Vernaux *) vstart;

                        aux.vna_hash  = BYTE_GET (eaux->vna_hash);
                        aux.vna_flags = BYTE_GET (eaux->vna_flags);
                        aux.vna_other = BYTE_GET (eaux->vna_other);
                        aux.vna_name  = BYTE_GET (eaux->vna_name);
                        aux.vna_next  = BYTE_GET (eaux->vna_next);

                        if (dynamic_strings)
                            printf ("  %#06x: Name: %s",
                                    isum, dynamic_strings + aux.vna_name);
                        else
                            printf ("  %#06x: Name index: %lx",
                                    isum, aux.vna_name);

                        printf ("  Flags: %s  Version: %d\n",
                                get_ver_flags (aux.vna_flags), aux.vna_other);

                        isum   += aux.vna_next;
                        vstart += aux.vna_next;
                    }

                    idx += ent.vn_next;
                }

                free (eneed);
            }
            break;

        case SHT_GNU_versym:
            {
                Elf32_Internal_Shdr *       link_section;
                int                    total;
                int                    cnt;
                unsigned char *      edata;
                unsigned short *       data;
                char *                 strtab;
                Elf_Internal_Sym *     symbols;
                Elf32_Internal_Shdr *       string_sec;

                link_section = SECTION_HEADER (section->sh_link);
                total = section->sh_size / section->sh_entsize;

                found = 1;

                symbols = GET_ELF_SYMBOLS (file, link_section);

                string_sec = SECTION_HEADER (link_section->sh_link);

                strtab = (char *) get_data (NULL, file, string_sec->sh_offset,
                                            string_sec->sh_size,
                                            "version string table");
                if (!strtab)
                    break;

                printf ("\nVersion symbols section '%s' contains %d entries:\n",
                        SECTION_NAME (section), total);

                printf (" Addr: ");
                printf_vma (section->sh_addr);
                printf ("  Offset: %#08lx  Link: %lx (%s)\n",
                        (unsigned long) section->sh_offset, section->sh_link,
                        SECTION_NAME (link_section));

                edata =
                        ((unsigned char *)
                         get_data (NULL, file,
                                   version_info[DT_VERSIONTAGIDX (DT_VERSYM)] - loadaddr,
                                   total * sizeof (short), "version symbol data"));
                if (!edata)
                {
                    free (strtab);
                    break;
                }

                data = (unsigned short *) malloc (total * sizeof (short));

                for (cnt = total; cnt --;)
                    data [cnt] = byte_get (edata + cnt * sizeof (short),
                                           sizeof (short));

                free (edata);

                for (cnt = 0; cnt < total; cnt += 4)
                {
                    int j, nn;
                    int check_def, check_need;
                    char * name;

                    printf ("  %03x:", cnt);

                    for (j = 0; (j < 4) && (cnt + j) < total; ++j)
                        switch (data [cnt + j])
                        {
                        case 0:
                        fputs ("   0 (*local*)    ", stdout);
                        break;

                        case 1:
                        fputs ("   1 (*global*)   ", stdout);
                        break;

                        default:
                        nn = printf ("%4x%c", data [cnt + j] & 0x7fff,
                                     data [cnt + j] & 0x8000 ? 'h' : ' ');

                        check_def = 1;
                        check_need = 1;
                        if (SECTION_HEADER (symbols [cnt + j].st_shndx)->sh_type
                            != SHT_NOBITS)
                        {
                            if (symbols [cnt + j].st_shndx == SHN_UNDEF)
                                check_def = 0;
                            else
                                check_need = 0;
                        }

                        if (check_need
                            && version_info [DT_VERSIONTAGIDX (DT_VERNEED)])
                        {
                            Elf_Internal_Verneed     ivn;
                            unsigned long            offset;

                            offset = version_info [DT_VERSIONTAGIDX (DT_VERNEED)]
                                     - loadaddr;

                            do
                            {
                                Elf_Internal_Vernaux   ivna;
                                Elf_External_Verneed   evn;
                                Elf_External_Vernaux   evna;
                                unsigned long          a_off;

                                get_data (&evn, file, offset, sizeof (evn),
                                          "version need");

                                ivn.vn_aux  = BYTE_GET (evn.vn_aux);
                                ivn.vn_next = BYTE_GET (evn.vn_next);

                                a_off = offset + ivn.vn_aux;

                                do
                                {
                                    get_data (&evna, file, a_off, sizeof (evna),
                                              "version need aux (2)");

                                    ivna.vna_next  = BYTE_GET (evna.vna_next);
                                    ivna.vna_other = BYTE_GET (evna.vna_other);

                                    a_off += ivna.vna_next;
                                }
                                while (ivna.vna_other != data [cnt + j]
                                       && ivna.vna_next != 0);

                                if (ivna.vna_other == data [cnt + j])
                                {
                                    ivna.vna_name = BYTE_GET (evna.vna_name);

                                    name = strtab + ivna.vna_name;
                                    nn += printf ("(%s%-*s",
                                                  name,
                                                  12 - (int) strlen (name),
                                                  ")");
                                    check_def = 0;
                                    break;
                                }

                                offset += ivn.vn_next;
                            }
                            while (ivn.vn_next);
                        }

                        if (check_def && data [cnt + j] != 0x8001
                            && version_info [DT_VERSIONTAGIDX (DT_VERDEF)])
                        {
                            Elf_Internal_Verdef  ivd;
                            Elf_External_Verdef  evd;
                            unsigned long        offset;

                            offset = version_info
                                     [DT_VERSIONTAGIDX (DT_VERDEF)] - loadaddr;

                            do
                            {
                                get_data (&evd, file, offset, sizeof (evd),
                                          "version def");

                                ivd.vd_next = BYTE_GET (evd.vd_next);
                                ivd.vd_ndx  = BYTE_GET (evd.vd_ndx);

                                offset += ivd.vd_next;
                            }
                            while (ivd.vd_ndx != (data [cnt + j] & 0x7fff)
                                && ivd.vd_next != 0);

                            if (ivd.vd_ndx == (data [cnt + j] & 0x7fff))
                            {
                                Elf_External_Verdaux  evda;
                                Elf_Internal_Verdaux  ivda;

                                ivd.vd_aux = BYTE_GET (evd.vd_aux);

                                get_data (&evda, file,
                                          offset - ivd.vd_next + ivd.vd_aux,
                                          sizeof (evda), "version def aux");

                                ivda.vda_name = BYTE_GET (evda.vda_name);

                                name = strtab + ivda.vda_name;
                                nn += printf ("(%s%-*s",
                                              name,
                                              12 - (int) strlen (name),
                                              ")");
                            }
                        }

                        if (nn < 18)
                            printf ("%*c", 18 - nn, ' ');
                    }

                    putchar ('\n');
                }

                free (data);
                free (strtab);
                free (symbols);
            }
            break;

        default:
            break;
        }
    }

    if (! found)
        QMessageBox::warning(global_parent, "readELF warning", QString("No version information found in this file"),
                             QMessageBox::Ok, QMessageBox::NoButton);
    //printf ("\nNo version information found in this file.\n");

    return 1;
}


static char *get_file_type (unsigned e_type)
{
    static char buff [32];

    switch (e_type)
    {
    case ET_NONE:  return "NONE (None)";
    case ET_REL:  return "REL (Relocatable file)";
    case ET_EXEC:       return "EXEC (Executable file)";
    case ET_DYN:        return "DYN (Shared object file)";
    case ET_CORE:       return "CORE (Core file)";

    default:
        if ((e_type >= ET_LOPROC) && (e_type <= ET_HIPROC))
            sprintf (buff, "Processor Specific: (%x)", e_type);
        else if ((e_type >= ET_LOOS) && (e_type <= ET_HIOS))
            sprintf (buff, "OS Specific: (%x)", e_type);
        else
            sprintf (buff, "<unknown>: %x", e_type);
        return buff;
    }
}


static const char *get_ia64_segment_type(unsigned long type)
{
    switch (type)
    {
    case PT_IA_64_ARCHEXT: return "IA_64_ARCHEXT";
    case PT_IA_64_UNWIND:   return "IA_64_UNWIND";
    case PT_HP_TLS:     return "HP_TLS";
    case PT_IA_64_HP_OPT_ANOT:   return "HP_OPT_ANNOT";
    case PT_IA_64_HP_HSL_ANOT:   return "HP_HSL_ANNOT";
    case PT_IA_64_HP_STACK:   return "HP_STACK";
    default:
        break;
    }

    return NULL;
}


static const char *get_segment_type (unsigned long p_type)
{
    static char buff [32];

    switch (p_type)
    {
    case PT_NULL:       return "NULL";
    case PT_LOAD:       return "LOAD";
    case PT_DYNAMIC:  return "DYNAMIC";
    case PT_INTERP:     return "INTERP";
    case PT_NOTE:       return "NOTE";
    case PT_SHLIB:      return "SHLIB";
    case PT_PHDR:       return "PHDR";
    case PT_TLS:  return "TLS";

    case PT_GNU_EH_FRAME:
        return "GNU_EH_FRAME";

    default:
        if ((p_type >= PT_LOPROC) && (p_type <= PT_HIPROC))
        {
            const char * result;

            switch (elf_header.e_machine)
            {
            case EM_MIPS:
            case EM_MIPS_RS3_LE:
                //       result = get_mips_segment_type (p_type);
                break;
            case EM_PARISC:
                //      result = get_parisc_segment_type (p_type);
                break;
            case EM_IA_64:
                result = get_ia64_segment_type (p_type);
                break;
            default:
                result = NULL;
                break;
            }

            if (result != NULL)
                return result;

            sprintf (buff, "LOPROC+%lx", p_type - PT_LOPROC);
        }
        else if ((p_type >= PT_LOOS) && (p_type <= PT_HIOS))
        {
            const char * result;

            switch (elf_header.e_machine)
            {
            case EM_PARISC:
                //       result = get_parisc_segment_type (p_type);
                break;
            case EM_IA_64:
                result = get_ia64_segment_type (p_type);
                break;
            default:
                result = NULL;
                break;
            }

            if (result != NULL)
                return result;

            sprintf (buff, "LOOS+%lx", p_type - PT_LOOS);
        }
        else
            sprintf (buff, "<unknown>: %lx", p_type);

        return buff;
    }
}



static char *get_machine_flags (unsigned e_flags, unsigned e_machine)
{
    static char buf [1024];

    buf[0] = '\0';

    if (e_flags)
    {
        switch (e_machine)
        {
        default:
            break;
            /*
   case EM_ARM:
     decode_ARM_machine_flags (e_flags, buf);
     break;

   case EM_68K:
     if (e_flags & EF_CPU32)
       strcat (buf, ", cpu32");
     if (e_flags & EF_M68000)
       strcat (buf, ", m68000");
     break;
*/
        case EM_PPC:
            if (e_flags & EF_PPC_EMB)
                strcat (buf, ", emb");

            if (e_flags & EF_PPC_RELOCATABLE)
                strcat (buf, ", relocatable");

            if (e_flags & EF_PPC_RELOCATABLE_LIB)
                strcat (buf, ", relocatable-lib");
            break;
            /*
   case EM_V850:
   case EM_CYGNUS_V850:
     switch (e_flags & EF_V850_ARCH)
       {
       case E_V850E_ARCH:
         strcat (buf, ", v850e");
         break;
       case E_V850EA_ARCH:
         strcat (buf, ", v850ea");
         break;
       case E_V850_ARCH:
         strcat (buf, ", v850");
         break;
       default:
         strcat (buf, ", unknown v850 architecture variant");
         break;
       }
     break;

   case EM_M32R:
   case EM_CYGNUS_M32R:
     if ((e_flags & EF_M32R_ARCH) == E_M32R_ARCH)
       strcat (buf, ", m32r");

     break;

   case EM_MIPS:
   case EM_MIPS_RS3_LE:
     if (e_flags & EF_MIPS_NOREORDER)
       strcat (buf, ", noreorder");

     if (e_flags & EF_MIPS_PIC)
       strcat (buf, ", pic");

     if (e_flags & EF_MIPS_CPIC)
       strcat (buf, ", cpic");

     if (e_flags & EF_MIPS_UCODE)
       strcat (buf, ", ugen_reserved");

     if (e_flags & EF_MIPS_ABI2)
       strcat (buf, ", abi2");

     if (e_flags & EF_MIPS_OPTIONS_FIRST)
       strcat (buf, ", odk first");

     if (e_flags & EF_MIPS_32BITMODE)
       strcat (buf, ", 32bitmode");

     switch ((e_flags & EF_MIPS_MACH))
       {
       case E_MIPS_MACH_3900: strcat (buf, ", 3900"); break;
       case E_MIPS_MACH_4010: strcat (buf, ", 4010"); break;
       case E_MIPS_MACH_4100: strcat (buf, ", 4100"); break;
       case E_MIPS_MACH_4650: strcat (buf, ", 4650"); break;
       case E_MIPS_MACH_4111: strcat (buf, ", 4111"); break;
       case E_MIPS_MACH_SB1:  strcat (buf, ", sb1");  break;
       case 0:

         break;
       default: strcat (buf, ", unknown CPU"); break;
       }

     switch ((e_flags & EF_MIPS_ABI))
       {
       case E_MIPS_ABI_O32: strcat (buf, ", o32"); break;
       case E_MIPS_ABI_O64: strcat (buf, ", o64"); break;
       case E_MIPS_ABI_EABI32: strcat (buf, ", eabi32"); break;
       case E_MIPS_ABI_EABI64: strcat (buf, ", eabi64"); break;
       case 0:
         break;
       default: strcat (buf, ", unknown ABI"); break;
       }

     if (e_flags & EF_MIPS_ARCH_ASE_MDMX)
       strcat (buf, ", mdmx");

     if (e_flags & EF_MIPS_ARCH_ASE_M16)
       strcat (buf, ", mips16");

     switch ((e_flags & EF_MIPS_ARCH))
       {
       case E_MIPS_ARCH_1: strcat (buf, ", mips1"); break;
       case E_MIPS_ARCH_2: strcat (buf, ", mips2"); break;
       case E_MIPS_ARCH_3: strcat (buf, ", mips3"); break;
       case E_MIPS_ARCH_4: strcat (buf, ", mips4"); break;
       case E_MIPS_ARCH_5: strcat (buf, ", mips5"); break;
       case E_MIPS_ARCH_32: strcat (buf, ", mips32"); break;
       case E_MIPS_ARCH_64: strcat (buf, ", mips64"); break;
       default: strcat (buf, ", unknown ISA"); break;
       }

     break;
*/
        case EM_SPARCV9:
            if (e_flags & EF_SPARC_32PLUS)
                strcat (buf, ", v8+");

            if (e_flags & EF_SPARC_SUN_US1)
                strcat (buf, ", ultrasparcI");

            if (e_flags & EF_SPARC_SUN_US3)
                strcat (buf, ", ultrasparcIII");

            if (e_flags & EF_SPARC_HAL_R1)
                strcat (buf, ", halr1");

            if (e_flags & EF_SPARC_LEDATA)
                strcat (buf, ", ledata");

            if ((e_flags & EF_SPARCV9_MM) == EF_SPARCV9_TSO)
                strcat (buf, ", tso");

            if ((e_flags & EF_SPARCV9_MM) == EF_SPARCV9_PSO)
                strcat (buf, ", pso");

            if ((e_flags & EF_SPARCV9_MM) == EF_SPARCV9_RMO)
                strcat (buf, ", rmo");
            break;
            /*
   case EM_PARISC:
     switch (e_flags & EF_PARISC_ARCH)
       {
       case EFA_PARISC_1_0:
         strcpy (buf, ", PA-RISC 1.0");
         break;
       case EFA_PARISC_1_1:
         strcpy (buf, ", PA-RISC 1.1");
         break;
       case EFA_PARISC_2_0:
         strcpy (buf, ", PA-RISC 2.0");
         break;
       default:
         break;
       }
     if (e_flags & EF_PARISC_TRAPNIL)
       strcat (buf, ", trapnil");
     if (e_flags & EF_PARISC_EXT)
       strcat (buf, ", ext");
     if (e_flags & EF_PARISC_LSB)
       strcat (buf, ", lsb");
     if (e_flags & EF_PARISC_WIDE)
       strcat (buf, ", wide");
     if (e_flags & EF_PARISC_NO_KABP)
       strcat (buf, ", no kabp");
     if (e_flags & EF_PARISC_LAZYSWAP)
       strcat (buf, ", lazyswap");
     break;

   case EM_PJ:
   case EM_PJ_OLD:
     if ((e_flags & EF_PICOJAVA_NEWCALLS) == EF_PICOJAVA_NEWCALLS)
       strcat (buf, ", new calling convention");

     if ((e_flags & EF_PICOJAVA_GNUCALLS) == EF_PICOJAVA_GNUCALLS)
       strcat (buf, ", gnu calling convention");
     break;
*/
        case EM_IA_64:
            if ((e_flags & EF_IA_64_ABI64))
                strcat (buf, ", 64-bit");
            else
                strcat (buf, ", 32-bit");
            if ((e_flags & EF_IA_64_REDUCEDFP))
                strcat (buf, ", reduced fp model");
            if ((e_flags & EF_IA_64_NOFUNCDESC_CONS_GP))
                strcat (buf, ", no function descriptors, constant gp");
            else if ((e_flags & EF_IA_64_CONS_GP))
                strcat (buf, ", constant gp");
            if ((e_flags & EF_IA_64_ABSOLUTE))
                strcat (buf, ", absolute");
            break;

            /* case EM_VAX:
     if ((e_flags & EF_VAX_NONPIC))
       strcat (buf, ", non-PIC");
     if ((e_flags & EF_VAX_DFLOAT))
       strcat (buf, ", D-Float");
     if ((e_flags & EF_VAX_GFLOAT))
       strcat (buf, ", G-Float");
     break;*/
        }
    }

    return buf;
}


static char *get_machine_name (unsigned e_machine)
{
    static char buff [64]; /* XXX */

    switch (e_machine)
    {
    case EM_NONE:    return "None";
    case EM_M32:     return "WE32100";
    case EM_SPARC:      return "Sparc";
    case EM_386:     return "Intel 80386";
    case EM_68K:     return "MC68000";
    case EM_88K:     return "MC88000";
    case EM_486:     return "Intel 80486";
    case EM_860:     return "Intel 80860";
    case EM_MIPS:    return "MIPS R3000";
    case EM_S370:    return "IBM System/370";
    case EM_MIPS_RS3_LE:   return "MIPS R4000 big-endian";
    case EM_OLD_SPARCV9:   return "Sparc v9 (old)";
    case EM_PARISC:     return "HPPA";
    case EM_PPC_OLD:    return "Power PC (old)";
    case EM_SPARC32PLUS:   return "Sparc v8+" ;
    case EM_960:     return "Intel 90860";
    case EM_PPC:     return "PowerPC";
    case EM_PPC64:      return "PowerPC64";
    case EM_V800:    return "NEC V800";
    case EM_FR20:    return "Fujitsu FR20";
    case EM_RH32:    return "TRW RH32";
    case EM_MCORE:           return "MCORE";
    case EM_ARM:     return "ARM";
    case EM_OLD_ALPHA:     return "Digital Alpha (old)";
    case EM_SH:         return "Hitachi SH";
    case EM_SPARCV9:    return "Sparc v9";
    case EM_TRICORE:    return "Siemens Tricore";
    case EM_ARC:     return "ARC";
    case EM_H8_300:     return "Hitachi H8/300";
    case EM_H8_300H:    return "Hitachi H8/300H";
    case EM_H8S:     return "Hitachi H8S";
    case EM_H8_500:     return "Hitachi H8/500";
    case EM_IA_64:      return "Intel IA-64";
    case EM_MIPS_X:     return "Stanford MIPS-X";
    case EM_COLDFIRE:      return "Motorola Coldfire";
    case EM_68HC12:     return "Motorola M68HC12";
    case EM_ALPHA:      return "Alpha";
    case EM_CYGNUS_D10V:
    case EM_D10V:    return "d10v";
    case EM_CYGNUS_D30V:
    case EM_D30V:          return "d30v";
    case EM_CYGNUS_M32R:
    case EM_M32R:    return "Mitsubishi M32r";
    case EM_CYGNUS_V850:
    case EM_V850:    return "NEC v850";
    case EM_CYGNUS_MN10300:
    case EM_MN10300:    return "mn10300";
    case EM_CYGNUS_MN10200:
    case EM_MN10200:    return "mn10200";
    case EM_CYGNUS_FR30:
    case EM_FR30:    return "Fujitsu FR30";
    case EM_CYGNUS_FRV:          return "Fujitsu FR-V";
    case EM_PJ_OLD:
    case EM_PJ:                 return "picoJava";
    case EM_MMA:     return "Fujitsu Multimedia Accelerator";
    case EM_PCP:     return "Siemens PCP";
    case EM_NCPU:    return "Sony nCPU embedded RISC processor";
    case EM_NDR1:    return "Denso NDR1 microprocesspr";
    case EM_STARCORE:      return "Motorola Star*Core processor";
    case EM_ME16:    return "Toyota ME16 processor";
    case EM_ST100:      return "STMicroelectronics ST100 processor";
    case EM_TINYJ:      return "Advanced Logic Corp. TinyJ embedded processor";
    case EM_FX66:    return "Siemens FX66 microcontroller";
    case EM_ST9PLUS:    return "STMicroelectronics ST9+ 8/16 bit microcontroller";
    case EM_ST7:     return "STMicroelectronics ST7 8-bit microcontroller";
    case EM_68HC16:     return "Motorola MC68HC16 Microcontroller";
    case EM_68HC11:     return "Motorola MC68HC11 Microcontroller";
    case EM_68HC08:     return "Motorola MC68HC08 Microcontroller";
    case EM_68HC05:     return "Motorola MC68HC05 Microcontroller";
    case EM_SVX:     return "Silicon Graphics SVx";
    case EM_ST19:    return "STMicroelectronics ST19 8-bit microcontroller";
    case EM_VAX:     return "Digital VAX";
    case EM_AVR_OLD:
    case EM_AVR:                return "Atmel AVR 8-bit microcontroller";
    case EM_CRIS:    return "Axis Communications 32-bit embedded processor";
    case EM_JAVELIN:    return "Infineon Technologies 32-bit embedded cpu";
    case EM_FIREPATH:      return "Element 14 64-bit DSP processor";
    case EM_ZSP:     return "LSI Logic's 16-bit DSP processor";
    case EM_MMIX:          return "Donald Knuth's educational 64-bit processor";
    case EM_HUANY:      return "Harvard Universitys's machine-independent object format";
    case EM_PRISM:      return "SiTera Prism";
    case EM_X86_64:     return "Advanced Micro Devices X86-64";
    case EM_S390_OLD:
    case EM_S390:               return "IBM S/390";
    case EM_XSTORMY16:     return "Sanyo Xstormy16 CPU core";
    case EM_OPENRISC:
    case EM_OR32:    return "OpenRISC";
    case EM_DLX:     return "OpenDLX";
    default:
        sprintf (buff, "<unknown>: %x", e_machine);
        return buff;
    }
}
static int get_64bit_section_headers (FILE * file ,     unsigned int num)
{
    Elf64_External_Shdr * shdrs;
    Elf64_Internal_Shdr * internal;
    unsigned int          i;

    shdrs = ((Elf64_External_Shdr *)
             get_data (NULL, file, elf_header.e_shoff,
                       elf_header.e_shentsize * num,
                       "section headers"));
    if (!shdrs)
        return 0;

    section_headers = ((Elf_Internal_Shdr *)
                       malloc (num * sizeof (Elf_Internal_Shdr)));

    if (section_headers == NULL)
    {
        QMessageBox::warning(global_parent, "readELF warning", QString("Out of memory"),
                             QMessageBox::Ok, QMessageBox::NoButton);

        //printf("Out of memory\n");
        return 0;
    }

    for (i = 0, internal = section_headers;
         i < num;
         i ++, internal ++)
    {
        internal->sh_name      = BYTE_GET (shdrs[i].sh_name);
        internal->sh_type      = BYTE_GET (shdrs[i].sh_type);
        internal->sh_flags     = BYTE_GET8 (shdrs[i].sh_flags);
        internal->sh_addr      = BYTE_GET8 (shdrs[i].sh_addr);
        internal->sh_size      = BYTE_GET8 (shdrs[i].sh_size);
        internal->sh_entsize   = BYTE_GET8 (shdrs[i].sh_entsize);
        internal->sh_link      = BYTE_GET (shdrs[i].sh_link);
        internal->sh_info      = BYTE_GET (shdrs[i].sh_info);
        internal->sh_offset    = BYTE_GET (shdrs[i].sh_offset);
        internal->sh_addralign = BYTE_GET (shdrs[i].sh_addralign);
    }

    free (shdrs);

    return 1;
}




static int get_32bit_dynamic_segment (FILE * file)
{
    Elf32_External_Dyn * edyn;
    Elf_Internal_Dyn *   entry;
    bfd_size_type        i;

    edyn = (Elf32_External_Dyn *) get_data (NULL, file, dynamic_addr,
                                            dynamic_size, "dynamic segment");
    if (!edyn)
        return 0;

    /* SGI's ELF has more than one section in the DYNAMIC segment.  Determine
     how large this .dynamic is now.  We can do this even before the byte
     swapping since the DT_NULL tag is recognizable.  */
    dynamic_size = 0;
    while (*(Elf32_Word *) edyn [dynamic_size++].d_tag != DT_NULL)
        ;

    dynamic_segment = (Elf_Internal_Dyn *)
                      malloc (dynamic_size * sizeof (Elf_Internal_Dyn));

    if (dynamic_segment == NULL)
    {
        QMessageBox::warning(global_parent, "readELF warning", QString("Out of memory"),
                             QMessageBox::Ok, QMessageBox::NoButton);

        //printf("Out of memory\n");
        free (edyn);
        return 0;
    }

    for (i = 0, entry = dynamic_segment;
         i < dynamic_size;
         i ++, entry ++)
    {
        entry->d_tag      = BYTE_GET (edyn [i].d_tag);
        entry->d_un.d_val = BYTE_GET (edyn [i].d_un.d_val);
    }

    free (edyn);
    return 1;
}

static int
        get_64bit_dynamic_segment (     FILE * file)
{
    Elf64_External_Dyn * edyn;
    Elf_Internal_Dyn *   entry;
    bfd_size_type        i;

    edyn = (Elf64_External_Dyn *) get_data (NULL, file, dynamic_addr,
                                            dynamic_size, "dynamic segment");
    if (!edyn)
        return 0;

    /* SGI's ELF has more than one section in the DYNAMIC segment.  Determine
     how large this .dynamic is now.  We can do this even before the byte
     swapping since the DT_NULL tag is recognizable.  */
    dynamic_size = 0;
    while (*(bfd_vma *) edyn [dynamic_size ++].d_tag != DT_NULL)
        ;

    dynamic_segment = (Elf_Internal_Dyn *)
                      malloc (dynamic_size * sizeof (Elf_Internal_Dyn));

    if (dynamic_segment == NULL)
    {
        QMessageBox::warning(global_parent, "readELF warning", QString("Out of memory"),
                             QMessageBox::Ok, QMessageBox::NoButton);

        //printf("Out of memory\n");
        free (edyn);
        return 0;
    }

    for (i = 0, entry = dynamic_segment;
         i < dynamic_size;
         i ++, entry ++)
    {
        entry->d_tag      = BYTE_GET8 (edyn [i].d_tag);
        entry->d_un.d_val = BYTE_GET8 (edyn [i].d_un.d_val);
    }

    free (edyn);

    return 1;
}


static Elf_Internal_Sym *get_32bit_elf_symbols (FILE * file,     Elf_Internal_Shdr *section)
{
    unsigned long number;
    Elf32_External_Sym * esyms;
    Elf_External_Sym_Shndx *shndx;
    Elf_Internal_Sym *   isyms;
    Elf_Internal_Sym *   psym;
    unsigned int         j;

    esyms = ((Elf32_External_Sym *)
             get_data (NULL, file, section->sh_offset,
                       section->sh_size, "symbols"));
    if (!esyms)
        return NULL;

    shndx = NULL;
    if (symtab_shndx_hdr != NULL
        && (symtab_shndx_hdr->sh_link
            == (unsigned long) SECTION_HEADER_NUM (section - section_headers)))
    {
        shndx = ((Elf_External_Sym_Shndx *)
                 get_data (NULL, file, symtab_shndx_hdr->sh_offset,
                           symtab_shndx_hdr->sh_size, "symtab shndx"));
        if (!shndx)
        {
            free (esyms);
            return NULL;
        }
    }

    number = section->sh_size / section->sh_entsize;
    isyms = (Elf_Internal_Sym *) malloc (number * sizeof (Elf_Internal_Sym));

    if (isyms == NULL)
    {
        QMessageBox::warning(global_parent, "readELF warning", QString("Out of memory"),
                             QMessageBox::Ok, QMessageBox::NoButton);

        //      printf("Out of memory\n");
        if (shndx)
            free (shndx);
        free (esyms);
        return NULL;
    }

    for (j = 0, psym = isyms;
         j < number;
         j ++, psym ++)
    {
        psym->st_name  = BYTE_GET (esyms[j].st_name);
        psym->st_value = BYTE_GET (esyms[j].st_value);
        psym->st_size  = BYTE_GET (esyms[j].st_size);
        psym->st_shndx = BYTE_GET (esyms[j].st_shndx);
        if (psym->st_shndx == SHN_XINDEX && shndx != NULL)
            psym->st_shndx
                    = byte_get ((unsigned char *) &shndx[j], sizeof (shndx[j]));
        psym->st_info  = BYTE_GET (esyms[j].st_info);
        psym->st_other = BYTE_GET (esyms[j].st_other);
    }

    if (shndx)
        free (shndx);
    free (esyms);

    return isyms;
}

static Elf_Internal_Sym *get_64bit_elf_symbols ( FILE * file,      Elf_Internal_Shdr *section)
{
    unsigned long number;
    Elf64_External_Sym * esyms;
    Elf_External_Sym_Shndx *shndx;
    Elf_Internal_Sym *   isyms;
    Elf_Internal_Sym *   psym;
    unsigned int         j;

    esyms = ((Elf64_External_Sym *)
             get_data (NULL, file, section->sh_offset,
                       section->sh_size, "symbols"));
    if (!esyms)
        return NULL;

    shndx = NULL;
    if (symtab_shndx_hdr != NULL
        && (symtab_shndx_hdr->sh_link
            == (unsigned long) SECTION_HEADER_NUM (section - section_headers)))
    {
        shndx = ((Elf_External_Sym_Shndx *)
                 get_data (NULL, file, symtab_shndx_hdr->sh_offset,
                           symtab_shndx_hdr->sh_size, "symtab shndx"));
        if (!shndx)
        {
            free (esyms);
            return NULL;
        }
    }

    number = section->sh_size / section->sh_entsize;
    isyms = (Elf_Internal_Sym *) malloc (number * sizeof (Elf_Internal_Sym));

    if (isyms == NULL)
    {
        QMessageBox::warning(global_parent, "readELF warning", QString("Out of memory"),
                             QMessageBox::Ok, QMessageBox::NoButton);

        //      printf("Out of memory\n");
        if (shndx)
            free (shndx);
        free (esyms);
        return NULL;
    }

    for (j = 0, psym = isyms;
         j < number;
         j ++, psym ++)
    {
        psym->st_name  = BYTE_GET (esyms[j].st_name);
        psym->st_info  = BYTE_GET (esyms[j].st_info);
        psym->st_other = BYTE_GET (esyms[j].st_other);
        psym->st_shndx = BYTE_GET (esyms[j].st_shndx);
        if (psym->st_shndx == SHN_XINDEX && shndx != NULL)
            psym->st_shndx
                    = byte_get ((unsigned char *) &shndx[j], sizeof (shndx[j]));
        psym->st_value = BYTE_GET8 (esyms[j].st_value);
        psym->st_size  = BYTE_GET8 (esyms[j].st_size);
    }

    if (shndx)
        free (shndx);
    free (esyms);

    return isyms;
}


static const char *get_dynamic_flags (bfd_vma flags)
{
    static char buff [128];
    char *p = buff;

    *p = '\0';
    while (flags)
    {
        bfd_vma flag;

        flag = flags & - flags;
        flags &= ~ flag;

        if (p != buff)
            *p++ = ' ';

        switch (flag)
        {
        case DF_ORIGIN:   strcpy (p, "ORIGIN"); break;
        case DF_SYMBOLIC: strcpy (p, "SYMBOLIC"); break;
        case DF_TEXTREL:  strcpy (p, "TEXTREL"); break;
        case DF_BIND_NOW: strcpy (p, "BIND_NOW"); break;
        case DF_STATIC_TLS: strcpy (p, "STATIC_TLS"); break;
        default:          strcpy (p, "unknown"); break;
        }

        p = strchr (p, '\0');
    }
    return buff;
}



/* Parse and display the contents of the dynamic segment.  */
static int process_dynamic_segment(FILE * file)
{
    Elf_Internal_Dyn * entry;
    bfd_size_type      i;

    if (dynamic_size == 0)
    {
        if (do_dynamic)
            QMessageBox::warning(global_parent, "readELF warning", QString("There is no dynamic segment in this file"),
                                 QMessageBox::Ok, QMessageBox::NoButton);

        // printf ("\nThere is no dynamic segment in this file.\n");

        return 1;
    }

    if (is_32bit_elf)
    {
        if (! get_32bit_dynamic_segment (file))
            return 0;
    }
    else if (! get_64bit_dynamic_segment (file))
        return 0;

    /* Find the appropriate symbol table.  */
    if (dynamic_symbols == NULL)
    {
        for (i = 0, entry = dynamic_segment;
             i < dynamic_size;
             ++i, ++ entry)
        {
            Elf32_Internal_Shdr section;

            if (entry->d_tag != DT_SYMTAB)
                continue;

            dynamic_info[DT_SYMTAB] = entry->d_un.d_val;

            /* Since we do not know how big the symbol table is,
        we default to reading in the entire file (!) and
        processing that.  This is overkill, I know, but it
        should work.  */
            section.sh_offset = entry->d_un.d_val - loadaddr;

            if (fseek (file, 0, SEEK_END))
                QMessageBox::warning(global_parent, "readELF warning", QString("Unable to seek to end of file!"),
                                     QMessageBox::Ok, QMessageBox::NoButton);

            //printf("Unable to seek to end of file!");

            section.sh_size = ftell (file) - section.sh_offset;
            if (is_32bit_elf)
                section.sh_entsize = sizeof (Elf32_External_Sym);
            else
                section.sh_entsize = sizeof (Elf64_External_Sym);

            num_dynamic_syms = section.sh_size / section.sh_entsize;
            if (num_dynamic_syms < 1)
            {
                QMessageBox::warning(global_parent, "readELF warning", QString("Unable to determine the number of symbols to load"),
                                     QMessageBox::Ok, QMessageBox::NoButton);

                //printf("Unable to determine the number of symbols to load\n");
                continue;
            }

            dynamic_symbols = GET_ELF_SYMBOLS (file, &section);
        }
    }

    /* Similarly find a string table.  */
    if (dynamic_strings == NULL)
    {
        for (i = 0, entry = dynamic_segment;
             i < dynamic_size;
             ++i, ++ entry)
        {
            unsigned long offset;
            long          str_tab_len;

            if (entry->d_tag != DT_STRTAB)
                continue;

            dynamic_info[DT_STRTAB] = entry->d_un.d_val;

            /* Since we do not know how big the string table is,
        we default to reading in the entire file (!) and
        processing that.  This is overkill, I know, but it
        should work.  */

            offset = entry->d_un.d_val - loadaddr;
            if (fseek (file, 0, SEEK_END))
                QMessageBox::warning(global_parent, "readELF warning", QString("Unable to seek to end of file"),
                                     QMessageBox::Ok, QMessageBox::NoButton);

            //printf("Unable to seek to end of file\n");
            str_tab_len = ftell (file) - offset;

            if (str_tab_len < 1)
            {
                QMessageBox::warning(global_parent, "readELF warning", QString("Unable to determine the length of the dynamic string table"),
                                     QMessageBox::Ok, QMessageBox::NoButton);

                //       printf("Unable to determine the length of the dynamic string table\n");
                continue;
            }

            dynamic_strings = (char *) get_data (NULL, file, offset, str_tab_len,
                                                 "dynamic string table");
            break;
        }
    }

    /* And find the syminfo section if available.  */
    if (dynamic_syminfo == NULL)
    {
        unsigned int syminsz = 0;

        for (i = 0, entry = dynamic_segment;
             i < dynamic_size;
             ++i, ++ entry)
        {
            if (entry->d_tag == DT_SYMINENT)
            {
                /* Note: these braces are necessary to avoid a syntax
       error from the SunOS4 C compiler.  */
                assert (sizeof (Elf_External_Syminfo) == entry->d_un.d_val);
            }
            else if (entry->d_tag == DT_SYMINSZ)
                syminsz = entry->d_un.d_val;
            else if (entry->d_tag == DT_SYMINFO)
                dynamic_syminfo_offset = entry->d_un.d_val - loadaddr;
        }

        if (dynamic_syminfo_offset != 0 && syminsz != 0)
        {
            Elf_External_Syminfo * extsyminfo;
            Elf_Internal_Syminfo * syminfo;

            /* There is a syminfo section.  Read the data.  */
            extsyminfo = ((Elf_External_Syminfo *)
                          get_data (NULL, file, dynamic_syminfo_offset,
                                    syminsz, "symbol information"));
            if (!extsyminfo)
                return 0;

            dynamic_syminfo = (Elf_Internal_Syminfo *) malloc (syminsz);
            if (dynamic_syminfo == NULL)
            {
                QMessageBox::warning(global_parent, "readELF warning", QString("Out of memory"),
                                     QMessageBox::Ok, QMessageBox::NoButton);

                // printf("Out of memory\n");
                return 0;
            }

            dynamic_syminfo_nent = syminsz / sizeof (Elf_External_Syminfo);
            for (i = 0, syminfo = dynamic_syminfo; i < dynamic_syminfo_nent;
                 ++i, ++syminfo)
            {
                syminfo->si_boundto = BYTE_GET (extsyminfo[i].si_boundto);
                syminfo->si_flags = BYTE_GET (extsyminfo[i].si_flags);
            }

            free (extsyminfo);
        }
    }

    if (do_dynamic && dynamic_addr)
        /*    printf ("\nDynamic segment at offset 0x%x contains %ld entries:\n",
       dynamic_addr, (long) dynamic_size);*/
        if (do_dynamic)
            ;//printf ("  Tag        Type                         Name/Value\n");

    for (i = 0, entry = dynamic_segment;
         i < dynamic_size;
         i++, entry ++)
    {
        if (do_dynamic)
        {
            const char * dtype;

            //putchar (' ');
            //print_vma (entry->d_tag, FULL_HEX);
            dtype = get_dynamic_type (entry->d_tag);
            // printf (" (%s)%*s", dtype,  ((is_32bit_elf ? 27 : 19)  - (int) strlen (dtype)),  " ");
        }

        switch (entry->d_tag)
        {
        case DT_FLAGS:
            if (do_dynamic)
                ;//puts (get_dynamic_flags (entry->d_un.d_val));
            break;

        case DT_AUXILIARY:
        case DT_FILTER:
        case DT_CONFIG:
        case DT_DEPAUDIT:
        case DT_AUDIT:
            if (do_dynamic)
            {
                switch (entry->d_tag)
                {
                case DT_AUXILIARY:
                    printf ("Auxiliary library");
                    break;

                case DT_FILTER:
                    printf ("Filter library");
                    break;

                case DT_CONFIG:
                    printf ("Configuration file");
                    break;

                case DT_DEPAUDIT:
                    printf ("Dependency audit library");
                    break;

                case DT_AUDIT:
                    printf ("Audit library");
                    break;
                }

                if (dynamic_strings)
                    ;//printf (": [%s]\n", dynamic_strings + entry->d_un.d_val);
                else
                {
                    //printf (": ");
                    //print_vma (entry->d_un.d_val, PREFIX_HEX);
                    ;//putchar ('\n');
                }
            }
            break;

   case DT_FEATURE:
            if (do_dynamic)
            {
                //printf ("Flags:");
                if (entry->d_un.d_val == 0)
                    ;//printf (" None\n");
                else
                {
                    unsigned long int val = entry->d_un.d_val;
                    if (val & DTF_1_PARINIT)
                    {
                        //printf (" PARINIT");
                        val ^= DTF_1_PARINIT;
                    }
                    if (val & DTF_1_CONFEXP)
                    {
                        //printf (" CONFEXP");
                        val ^= DTF_1_CONFEXP;
                    }
                    if (val != 0)
                        //printf (" %lx", val);
                        ;//puts ("");
                }
            }
            break;

   case DT_POSFLAG_1:
            if (do_dynamic)
            {
                //printf ("Flags:");
                if (entry->d_un.d_val == 0)
                    ;//printf (" None\n");
                else
                {
                    unsigned long int val = entry->d_un.d_val;
                    if (val & DF_P1_LAZYLOAD)
                    {
                        //printf (" LAZYLOAD");
                        val ^= DF_P1_LAZYLOAD;
                    }
                    if (val & DF_P1_GROUPPERM)
                    {
                        //printf (" GROUPPERM");
                        val ^= DF_P1_GROUPPERM;
                    }
                    if (val != 0)
                        ;//printf (" %lx", val);
                    //puts ("");
                }
            }
            break;

   case DT_FLAGS_1:
            if (do_dynamic)
            {
                //printf ("Flags:");
                if (entry->d_un.d_val == 0)
                    ;//printf (" None\n");
                else
                {
                    unsigned long int val = entry->d_un.d_val;
                    if (val & DF_1_NOW)
                    {
                        //printf (" NOW");
                        val ^= DF_1_NOW;
                    }
                    if (val & DF_1_GLOBAL)
                    {
                        //printf (" GLOBAL");
                        val ^= DF_1_GLOBAL;
                    }
                    if (val & DF_1_GROUP)
                    {
                        //printf (" GROUP");
                        val ^= DF_1_GROUP;
                    }
                    if (val & DF_1_NODELETE)
                    {
                        //printf (" NODELETE");
                        val ^= DF_1_NODELETE;
                    }
                    if (val & DF_1_LOADFLTR)
                    {
                        //printf (" LOADFLTR");
                        val ^= DF_1_LOADFLTR;
                    }
                    if (val & DF_1_INITFIRST)
                    {
                        //printf (" INITFIRST");
                        val ^= DF_1_INITFIRST;
                    }
                    if (val & DF_1_NOOPEN)
                    {
                        //printf (" NOOPEN");
                        val ^= DF_1_NOOPEN;
                    }
                    if (val & DF_1_ORIGIN)
                    {
                        //printf (" ORIGIN");
                        val ^= DF_1_ORIGIN;
                    }
                    if (val & DF_1_DIRECT)
                    {
                        //printf (" DIRECT");
                        val ^= DF_1_DIRECT;
                    }
                    if (val & DF_1_TRANS)
                    {
                        // printf (" TRANS");
                        val ^= DF_1_TRANS;
                    }
                    if (val & DF_1_INTERPOSE)
                    {
                        // printf (" INTERPOSE");
                        val ^= DF_1_INTERPOSE;
                    }
                    if (val & DF_1_NODEFLIB)
                    {
                        // printf (" NODEFLIB");
                        val ^= DF_1_NODEFLIB;
                    }
                    if (val & DF_1_NODUMP)
                    {
                        // printf (" NODUMP");
                        val ^= DF_1_NODUMP;
                    }
                    if (val & DF_1_CONLFAT)
                    {
                        // printf (" CONLFAT");
                        val ^= DF_1_CONLFAT;
                    }
                    if (val != 0)
                        ;//printf (" %lx", val);
                    //puts ("");
                }
            }
            break;

   case DT_PLTREL:
            if (do_dynamic)
                ;//puts (get_dynamic_type (entry->d_un.d_val));
            break;

   case DT_NULL   :
   case DT_NEEDED  :
   case DT_PLTGOT  :
   case DT_HASH   :
   case DT_STRTAB  :
   case DT_SYMTAB  :
   case DT_RELA   :
   case DT_INIT   :
   case DT_FINI   :
   case DT_SONAME  :
   case DT_RPATH  :
   case DT_SYMBOLIC:
   case DT_REL  :
   case DT_DEBUG  :
   case DT_TEXTREL   :
   case DT_JMPREL  :
   case DT_RUNPATH   :
            dynamic_info[entry->d_tag] = entry->d_un.d_val;

            if (do_dynamic)
            {
                char * name;

                if (dynamic_strings == NULL)
                    name = NULL;
                else
                    name = dynamic_strings + entry->d_un.d_val;

                if (name)
                {
                    switch (entry->d_tag)
                    {
                    case DT_NEEDED:
                        // printf ("Shared library: [%s]", name);

                        // by me
                        //if(rpath)
                        neededLibVector.push_back(QString(name));

                        if (strcmp (name, program_interpreter) == 0)
                            ;//printf (" program interpreter");
                        break;

                    case DT_SONAME:
                        ;//printf ("Library soname: [%s]", name);
                        break;

                    case DT_RPATH:
                        rpathVector.push_back(QString(name));
                        //             cout << "rpathvector " << rpathVector[0] ;
                        //             printf (" Library rpath: [%s]", name);
                        break;

                    case DT_RUNPATH:
                        ;
                        //             printf ("Library runpath: [%s]", name);
                        break;

                    default:
                        ;//print_vma (entry->d_un.d_val, PREFIX_HEX);
                        break;
                    }
                }
                else
                    ;//print_vma (entry->d_un.d_val, PREFIX_HEX);

                //putchar ('\n');
            }
            break;

   case DT_PLTRELSZ:
   case DT_RELASZ  :
   case DT_STRSZ  :
   case DT_RELSZ  :
   case DT_RELAENT   :
   case DT_SYMENT  :
   case DT_RELENT  :
   case DT_PLTPADSZ:
   case DT_MOVEENT   :
   case DT_MOVESZ  :
   case DT_INIT_ARRAYSZ:
   case DT_FINI_ARRAYSZ:
   case DT_GNU_CONFLICTSZ:
   case DT_GNU_LIBLISTSZ:
            if (do_dynamic)
            {
                ;//print_vma (entry->d_un.d_val, UNSIGNED);
                //printf (" (bytes)\n");
            }
            break;

   case DT_VERDEFNUM:
   case DT_VERNEEDNUM:
   case DT_RELACOUNT:
   case DT_RELCOUNT:
            if (do_dynamic)
            {
                //print_vma (entry->d_un.d_val, UNSIGNED);
                ;//putchar ('\n');
            }
            break;

   case DT_SYMINSZ:
   case DT_SYMINENT:
   case DT_SYMINFO:
   case DT_USED:
   case DT_INIT_ARRAY:
   case DT_FINI_ARRAY:
            if (do_dynamic)
            {
                if (dynamic_strings != NULL && entry->d_tag == DT_USED)
                {
                    char * name;

                    name = dynamic_strings + entry->d_un.d_val;

                    if (* name)
                    {
                        printf ("Not needed object: [%s]\n", name);
                        break;
                    }
                }

                ;//print_vma (entry->d_un.d_val, PREFIX_HEX);
                //putchar ('\n');
            }
            break;

   case DT_BIND_NOW:
            /* The value of this entry is ignored.  */
            break;

   case DT_GNU_PRELINKED:
            if (do_dynamic)
            {
                struct tm * tmp;
                time_t time = entry->d_un.d_val;

                tmp = gmtime (&time);
                /*printf ("%04u-%02u-%02uT%02u:%02u:%02u\n",
            tmp->tm_year + 1900, tmp->tm_mon + 1, tmp->tm_mday,
            tmp->tm_hour, tmp->tm_min, tmp->tm_sec);*/

            }
            break;

   default:
            if ((entry->d_tag >= DT_VERSYM) && (entry->d_tag <= DT_VERNEEDNUM))
                version_info [DT_VERSIONTAGIDX (entry->d_tag)] =
                        entry->d_un.d_val;

            if (do_dynamic)
            {
                switch (elf_header.e_machine)
                {
                case EM_MIPS:
                case EM_MIPS_RS3_LE:
                    //    dynamic_segment_mips_val (entry);
                    break;
                case EM_PARISC:
                    //      dynamic_segment_parisc_val (entry);
                    break;
                default:
                    ;//print_vma (entry->d_un.d_val, PREFIX_HEX);
                    //putchar ('\n');
                }
            }
            break;
        }
    }

    return 1;
}







static int process_program_headers(FILE * file)
{
    Elf_Internal_Phdr * program_headers;
    Elf_Internal_Phdr * segment;
    unsigned int        i;

    if (elf_header.e_phnum == 0)
    {
        if (do_segments)
            QMessageBox::warning(global_parent, "readELF warning", QString("There are no program headers in this file"),
                                 QMessageBox::Ok, QMessageBox::NoButton);

        // printf ("\nThere are no program headers in this file.\n");
        return 1;
    }

    if (do_segments && !do_header)
    {
        printf ("\nElf file type is %s\n", get_file_type (elf_header.e_type));
        //      printf ("Entry point ");
        print_vma ((bfd_vma) elf_header.e_entry, PREFIX_HEX);
        printf ("\nThere are %d program headers, starting at offset ",
                elf_header.e_phnum);
        print_vma ((bfd_vma) elf_header.e_phoff, DEC);
        //      printf ("\n");
    }

    program_headers = (Elf_Internal_Phdr *) malloc
                      (elf_header.e_phnum * sizeof (Elf_Internal_Phdr));

    if (program_headers == NULL)
    {
        QMessageBox::warning(global_parent, "readELF warning", QString("Out of memory"),
                             QMessageBox::Ok, QMessageBox::NoButton);
        //printf("Out of memory\n");
        return 0;
    }

    if (is_32bit_elf)
        i = get_32bit_program_headers (file, program_headers);
    else
        i = get_64bit_program_headers (file, program_headers);

    if (i == 0)
    {
        free (program_headers);
        return 0;
    }

    if (do_segments)
    {
        if (elf_header.e_phnum > 1)
            printf ("\nProgram Headers:\n");
        else
            printf ("\nProgram Headers:\n");

        if (is_32bit_elf)
            printf
                    ("  Type           Offset   VirtAddr   PhysAddr   FileSiz MemSiz  Flg Align\n");
        else if (do_wide)
            printf
                    ("  Type           Offset   VirtAddr           PhysAddr           FileSiz  MemSiz   Flg Align\n");
        else
        {
            printf
                    ("  Type           Offset             VirtAddr           PhysAddr\n");
            printf
                    ("                 FileSiz            MemSiz              Flags  Align\n");
        }
    }

    loadaddr = -1;
    dynamic_addr = 0;
    dynamic_size = 0;

    for (i = 0, segment = program_headers;
         i < elf_header.e_phnum;
         i ++, segment ++)
    {
        if (do_segments)
        {
            printf ("  %-14.14s ", get_segment_type (segment->p_type));

            if (is_32bit_elf)
            {
                printf ("0x%6.6lx ", (unsigned long) segment->p_offset);
                printf ("0x%8.8lx ", (unsigned long) segment->p_vaddr);
                printf ("0x%8.8lx ", (unsigned long) segment->p_paddr);
                printf ("0x%5.5lx ", (unsigned long) segment->p_filesz);
                printf ("0x%5.5lx ", (unsigned long) segment->p_memsz);
                printf ("%c%c%c ",
                        (segment->p_flags & PF_R ? 'R' : ' '),
                        (segment->p_flags & PF_W ? 'W' : ' '),
                        (segment->p_flags & PF_X ? 'E' : ' '));
                printf ("%#lx", (unsigned long) segment->p_align);
            }
            else if (do_wide)
            {
                if ((unsigned long) segment->p_offset == segment->p_offset)
                    printf ("0x%6.6lx ", (unsigned long) segment->p_offset);
                else
                {
                    print_vma (segment->p_offset, FULL_HEX);
                    putchar (' ');
                }

                print_vma (segment->p_vaddr, FULL_HEX);
                putchar (' ');
                print_vma (segment->p_paddr, FULL_HEX);
                putchar (' ');

                if ((unsigned long) segment->p_filesz == segment->p_filesz)
                    printf ("0x%6.6lx ", (unsigned long) segment->p_filesz);
                else
                {
                    print_vma (segment->p_filesz, FULL_HEX);
                    putchar (' ');
                }

                if ((unsigned long) segment->p_memsz == segment->p_memsz)
                    printf ("0x%6.6lx", (unsigned long) segment->p_memsz);
                else
                {
                    print_vma (segment->p_offset, FULL_HEX);
                }

                printf (" %c%c%c ",
                        (segment->p_flags & PF_R ? 'R' : ' '),
                        (segment->p_flags & PF_W ? 'W' : ' '),
                        (segment->p_flags & PF_X ? 'E' : ' '));

                if ((unsigned long) segment->p_align == segment->p_align)
                    printf ("%#lx", (unsigned long) segment->p_align);
                else
                {
                    print_vma (segment->p_align, PREFIX_HEX);
                }
            }
            else
            {
                print_vma (segment->p_offset, FULL_HEX);
                putchar (' ');
                print_vma (segment->p_vaddr, FULL_HEX);
                putchar (' ');
                print_vma (segment->p_paddr, FULL_HEX);
                printf ("\n                 ");
                print_vma (segment->p_filesz, FULL_HEX);
                putchar (' ');
                print_vma (segment->p_memsz, FULL_HEX);
                printf ("  %c%c%c    ",
                        (segment->p_flags & PF_R ? 'R' : ' '),
                        (segment->p_flags & PF_W ? 'W' : ' '),
                        (segment->p_flags & PF_X ? 'E' : ' '));
                print_vma (segment->p_align, HEX);
            }
        }

        switch (segment->p_type)
        {
        case PT_LOAD:
            if (loadaddr == -1)
                loadaddr = (segment->p_vaddr & 0xfffff000)
                - (segment->p_offset & 0xfffff000);
            break;

        case PT_DYNAMIC:
            if (dynamic_addr)
                printf("more than one dynamic segment\n");

            dynamic_addr = segment->p_offset;
            dynamic_size = segment->p_filesz;
            break;

        case PT_INTERP:
            if (fseek (file, (long) segment->p_offset, SEEK_SET))
                QMessageBox::warning(global_parent, "readELF warning", QString("Unable to find program interpreter name"),
                                     QMessageBox::Ok, QMessageBox::NoButton);

            //printf("Unable to find program interpreter name\n");
            else
            {
                program_interpreter[0] = 0;
                fscanf (file, "%63s", program_interpreter);

                if (do_segments)
                    printf ("\n      [Requesting program interpreter: %s]",
                            program_interpreter);
            }
            break;
        }

        if (do_segments)
            putc ('\n', stdout);
    }

    if (loadaddr == -1)
    {
        /* Very strange.  */
        loadaddr = 0;
    }

    if (do_segments && section_headers != NULL)
    {
        printf ("\n Section to Segment mapping:\n");
        printf ("  Segment Sections...\n");

        assert (string_table != NULL);

        for (i = 0; i < elf_header.e_phnum; i++)
        {
            unsigned int j;
            Elf_Internal_Shdr * section;

            segment = program_headers + i;
            section = section_headers;

            printf ("   %2.2d     ", i);

            for (j = 1; j < elf_header.e_shnum; j++, section ++)
            {
                if (section->sh_size > 0
                    /* Compare allocated sections by VMA, unallocated
           sections by file offset.  */
                    && (section->sh_flags & SHF_ALLOC
                        ? (section->sh_addr >= segment->p_vaddr
                           && section->sh_addr + section->sh_size
                           <= segment->p_vaddr + segment->p_memsz)
                               : ((bfd_vma) section->sh_offset >= segment->p_offset
                                  && (section->sh_offset + section->sh_size
                                      <= segment->p_offset + segment->p_filesz))))
                    printf ("%s ", SECTION_NAME (section));
            }

            putc ('\n',stdout);
        }
    }

    free (program_headers);

    return 1;
}


static int get_32bit_section_headers(FILE * file, unsigned int num)
{
    Elf32_External_Shdr * shdrs;
    Elf32_Internal_Shdr * internal;
    unsigned int          i;

    shdrs = ((Elf32_External_Shdr *)
             get_data (NULL, file, elf_header.e_shoff,
                       elf_header.e_shentsize * num,
                       "section headers"));
    if (!shdrs)
        return 0;

    section_headers = ((Elf_Internal_Shdr *)
                       malloc (num * sizeof (Elf_Internal_Shdr)));

    if (section_headers == NULL)
    {
        QMessageBox::warning(global_parent, "readELF warning", QString("Out of memory"),
                             QMessageBox::Ok, QMessageBox::NoButton);

        //printf("Out of memory\n");
        return 0;
    }

    for (i = 0, internal = section_headers;
         i < num;
         i ++, internal ++)
    {
        internal->sh_name      = BYTE_GET (shdrs[i].sh_name);
        internal->sh_type      = BYTE_GET (shdrs[i].sh_type);
        internal->sh_flags     = BYTE_GET (shdrs[i].sh_flags);
        internal->sh_addr      = BYTE_GET (shdrs[i].sh_addr);
        internal->sh_offset    = BYTE_GET (shdrs[i].sh_offset);
        internal->sh_size      = BYTE_GET (shdrs[i].sh_size);
        internal->sh_link      = BYTE_GET (shdrs[i].sh_link);
        internal->sh_info      = BYTE_GET (shdrs[i].sh_info);
        internal->sh_addralign = BYTE_GET (shdrs[i].sh_addralign);
        internal->sh_entsize   = BYTE_GET (shdrs[i].sh_entsize);
    }

    free (shdrs);

    return 1;
}



static const char *get_elf_class(unsigned int elf_class)
{
    static char buff [32];

    switch (elf_class)
    {
    case ELFCLASSNONE: return "none";
    case ELFCLASS32:   return "ELF32";
    case ELFCLASS64:   return "ELF64";
    default:
        sprintf (buff, "<unknown: %x>", elf_class);
        return buff;
    }
}


static const char *get_data_encoding(unsigned int encoding)
{
    static char buff [32];

    switch (encoding)
    {
    case ELFDATANONE: return "none";
    case ELFDATA2LSB: return "2's complement, little endian";
    case ELFDATA2MSB: return "2's complement, big endian";
    default:
        sprintf (buff, "<unknown: %x>", encoding);
        return buff;
    }
}


static const char *get_osabi_name(unsigned int osabi)
{
    static char buff [32];

    switch (osabi)
    {
    case ELFOSABI_NONE:       return "UNIX - System V";
    case ELFOSABI_HPUX:       return "UNIX - HP-UX";
    case ELFOSABI_NETBSD:     return "UNIX - NetBSD";
    case ELFOSABI_LINUX:      return "UNIX - Linux";
    case ELFOSABI_HURD:       return "GNU/Hurd";
    case ELFOSABI_SOLARIS:    return "UNIX - Solaris";
    case ELFOSABI_AIX:        return "UNIX - AIX";
    case ELFOSABI_IRIX:       return "UNIX - IRIX";
    case ELFOSABI_FREEBSD:    return "UNIX - FreeBSD";
    case ELFOSABI_TRU64:      return "UNIX - TRU64";
    case ELFOSABI_MODESTO:    return "Novell - Modesto";
    case ELFOSABI_OPENBSD:    return "UNIX - OpenBSD";
    case ELFOSABI_STANDALONE: return "Standalone App";
    case ELFOSABI_ARM:        return "ARM";
    default:
        sprintf (buff, "<unknown: %x>", osabi);
        return buff;
    }
}


/* Decode the data held in 'elf_header'.  */
static int process_file_header ()
{
    if (   elf_header.e_ident [EI_MAG0] != ELFMAG0
           || elf_header.e_ident [EI_MAG1] != ELFMAG1
           || elf_header.e_ident [EI_MAG2] != ELFMAG2
           || elf_header.e_ident [EI_MAG3] != ELFMAG3)
    {
        QMessageBox::critical(global_parent, "readELF error", "Not an ELF file - it has the wrong magic bytes at the start",
                              QMessageBox::Ok, QMessageBox::NoButton);

        //printf ("Not an ELF file - it has the wrong magic bytes at the start\n");
        return 0;
    }

    if (do_header)
    {
        int i;

        printf ("ELF Header:\n");
        printf ("  Magic:   ");
        for (i = 0; i < EI_NIDENT; i ++)
            printf ("%2.2x ", elf_header.e_ident [i]);
        printf ("\n");
        printf ("  Class:                             %s\n",
                get_elf_class (elf_header.e_ident [EI_CLASS]));
        printf ("  Data:                              %s\n",
                get_data_encoding (elf_header.e_ident [EI_DATA]));
        printf ("  Version:                           %d %s\n",
                elf_header.e_ident [EI_VERSION],
                (elf_header.e_ident [EI_VERSION] == EV_CURRENT
                 ? "(current)"
                     : (elf_header.e_ident [EI_VERSION] != EV_NONE
                        ? "<unknown: %lx>"
                            : "")));
        printf ("  OS/ABI:                            %s\n",
                get_osabi_name (elf_header.e_ident [EI_OSABI]));
        printf ("  ABI Version:                       %d\n",
                elf_header.e_ident [EI_ABIVERSION]);
        printf ("  Type:                              %s\n",
                get_file_type (elf_header.e_type));
        printf ("  Machine:                           %s\n",
                get_machine_name (elf_header.e_machine));
        printf ("  Version:                           0x%lx\n",
                (unsigned long) elf_header.e_version);

        printf ("  Entry point address:               ");
        print_vma ((bfd_vma) elf_header.e_entry, PREFIX_HEX);
        printf ("\n  Start of program headers:          ");
        print_vma ((bfd_vma) elf_header.e_phoff, DEC);
        printf (" (bytes into file)\n  Start of section headers:          ");
        print_vma ((bfd_vma) elf_header.e_shoff, DEC);
        printf (" (bytes into file)\n");

        printf ("  Flags:                             0x%lx%s\n",
                (unsigned long) elf_header.e_flags,
                get_machine_flags (elf_header.e_flags, elf_header.e_machine));
        printf ("  Size of this header:               %ld (bytes)\n",
                (long) elf_header.e_ehsize);
        printf ("  Size of program headers:           %ld (bytes)\n",
                (long) elf_header.e_phentsize);
        printf ("  Number of program headers:         %ld\n",
                (long) elf_header.e_phnum);
        printf ("  Size of section headers:           %ld (bytes)\n",
                (long) elf_header.e_shentsize);
        printf ("  Number of section headers:         %ld",
                (long) elf_header.e_shnum);
        if (section_headers != NULL && elf_header.e_shnum == 0)
            printf (" (%ld)", (long) section_headers[0].sh_size);
        putc ('\n', stdout);
        printf ("  Section header string table index: %ld",
                (long) elf_header.e_shstrndx);
        if (section_headers != NULL && elf_header.e_shstrndx == SHN_XINDEX)
            printf (" (%ld)", (long) section_headers[0].sh_link);
        putc ('\n', stdout);
    }

    if (section_headers != NULL)
    {
        if (elf_header.e_shnum == 0)
            elf_header.e_shnum = section_headers[0].sh_size;
        if (elf_header.e_shstrndx == SHN_XINDEX)
            elf_header.e_shstrndx = section_headers[0].sh_link;
        free (section_headers);
        section_headers = NULL;
    }

    return 1;
}


static int get_32bit_program_headers(FILE * file, Elf_Internal_Phdr * program_headers)
{
    Elf32_External_Phdr * phdrs;
    Elf32_External_Phdr * external;
    Elf32_Internal_Phdr * internal;
    unsigned int          i;

    phdrs = ((Elf32_External_Phdr *)
             get_data (NULL, file, elf_header.e_phoff,
                       elf_header.e_phentsize * elf_header.e_phnum,
                       "program headers"));
    if (!phdrs)
        return 0;

    for (i = 0, internal = program_headers, external = phdrs;
         i < elf_header.e_phnum;
         i ++, internal ++, external ++)
    {
        internal->p_type   = BYTE_GET (external->p_type);
        internal->p_offset = BYTE_GET (external->p_offset);
        internal->p_vaddr  = BYTE_GET (external->p_vaddr);
        internal->p_paddr  = BYTE_GET (external->p_paddr);
        internal->p_filesz = BYTE_GET (external->p_filesz);
        internal->p_memsz  = BYTE_GET (external->p_memsz);
        internal->p_flags  = BYTE_GET (external->p_flags);
        internal->p_align  = BYTE_GET (external->p_align);
    }

    free (phdrs);

    return 1;
}


static int get_64bit_program_headers(FILE * file, Elf_Internal_Phdr * program_headers)
{
    Elf64_External_Phdr * phdrs;
    Elf64_External_Phdr * external;
    Elf64_Internal_Phdr * internal;
    unsigned int          i;

    phdrs = ((Elf64_External_Phdr *)
             get_data (NULL, file, elf_header.e_phoff,
                       elf_header.e_phentsize * elf_header.e_phnum,
                       "program headers"));
    if (!phdrs)
        return 0;

    for (i = 0, internal = program_headers, external = phdrs;
         i < elf_header.e_phnum;
         i ++, internal ++, external ++)
    {
        internal->p_type   = BYTE_GET (external->p_type);
        internal->p_flags  = BYTE_GET (external->p_flags);
        internal->p_offset = BYTE_GET8 (external->p_offset);
        internal->p_vaddr  = BYTE_GET8 (external->p_vaddr);
        internal->p_paddr  = BYTE_GET8 (external->p_paddr);
        internal->p_filesz = BYTE_GET8 (external->p_filesz);
        internal->p_memsz  = BYTE_GET8 (external->p_memsz);
        internal->p_align  = BYTE_GET8 (external->p_align);
    }

    free (phdrs);

    return 1;
}


static bfd_vma byte_get_little_endian(unsigned char * field, int             size)
{
    switch (size)
    {
    case 1:
        return * field;

    case 2:
        return  ((unsigned int) (field [0]))
                |    (((unsigned int) (field [1])) << 8);

#ifndef BFD64
    case 8:
        /* We want to extract data from an 8 byte wide field and
    place it into a 4 byte wide field.  Since this is a little
    endian source we can juts use the 4 byte extraction code.  */
        /* Fall through.  */
#endif
    case 4:
        return  ((unsigned long) (field [0]))
                |    (((unsigned long) (field [1])) << 8)
                |    (((unsigned long) (field [2])) << 16)
                |    (((unsigned long) (field [3])) << 24);

#ifdef BFD64
    case 8:
    case -8:
        /* This is a special case, generated by the BYTE_GET8 macro.
    It means that we are loading an 8 byte value from a field
    in an external structure into an 8 byte value in a field
    in an internal strcuture.  */
        return  ((bfd_vma) (field [0]))
                |    (((bfd_vma) (field [1])) << 8)
                |    (((bfd_vma) (field [2])) << 16)
                |    (((bfd_vma) (field [3])) << 24)
                |    (((bfd_vma) (field [4])) << 32)
                |    (((bfd_vma) (field [5])) << 40)
                |    (((bfd_vma) (field [6])) << 48)
                |    (((bfd_vma) (field [7])) << 56);
#endif
    default:
        QMessageBox::warning(global_parent, "readELF warning", QString("Unhandled data length: %1").arg(size),
                             QMessageBox::Ok, QMessageBox::NoButton);

        //printf("Unhandled data length: %d\n", size);
        abort ();
    }
}


static bfd_vma byte_get_big_endian(unsigned char * field, int             size)
{
    switch (size)
    {
    case 1:
        return * field;

    case 2:
        return ((unsigned int) (field [1])) | (((int) (field [0])) << 8);

    case 4:
        return ((unsigned long) (field [3]))
                |   (((unsigned long) (field [2])) << 8)
                |   (((unsigned long) (field [1])) << 16)
                |   (((unsigned long) (field [0])) << 24);

#ifndef BFD64
    case 8:
        /* Although we are extracing data from an 8 byte wide field, we
    are returning only 4 bytes of data.  */
        return ((unsigned long) (field [7]))
                |   (((unsigned long) (field [6])) << 8)
                |   (((unsigned long) (field [5])) << 16)
                |   (((unsigned long) (field [4])) << 24);
#else
    case 8:
    case -8:
        /* This is a special case, generated by the BYTE_GET8 macro.
    It means that we are loading an 8 byte value from a field
    in an external structure into an 8 byte value in a field
    in an internal strcuture.  */
        return ((bfd_vma) (field [7]))
                |   (((bfd_vma) (field [6])) << 8)
                |   (((bfd_vma) (field [5])) << 16)
                |   (((bfd_vma) (field [4])) << 24)
                |   (((bfd_vma) (field [3])) << 32)
                |   (((bfd_vma) (field [2])) << 40)
                |   (((bfd_vma) (field [1])) << 48)
                |   (((bfd_vma) (field [0])) << 56);
#endif

    default:
        QMessageBox::warning(global_parent, "readELF warning", QString("Unhandled data length: %1").arg(size),
                             QMessageBox::Ok, QMessageBox::NoButton);

        //printf("Unhandled data length: %d\n", size);
        abort ();
    }
}

/*
static const char *get_mips_dynamic_type (unsigned long type)
{
  switch (type)
    {
    case DT_MIPS_RLD_VERSION: return "MIPS_RLD_VERSION";
    case DT_MIPS_TIME_STAMP: return "MIPS_TIME_STAMP";
    case DT_MIPS_ICHECKSUM: return "MIPS_ICHECKSUM";
    case DT_MIPS_IVERSION: return "MIPS_IVERSION";
    case DT_MIPS_FLAGS: return "MIPS_FLAGS";
    case DT_MIPS_BASE_ADDRESS: return "MIPS_BASE_ADDRESS";
    case DT_MIPS_MSYM: return "MIPS_MSYM";
    case DT_MIPS_CONFLICT: return "MIPS_CONFLICT";
    case DT_MIPS_LIBLIST: return "MIPS_LIBLIST";
    case DT_MIPS_LOCAL_GOTNO: return "MIPS_LOCAL_GOTNO";
    case DT_MIPS_CONFLICTNO: return "MIPS_CONFLICTNO";
    case DT_MIPS_LIBLISTNO: return "MIPS_LIBLISTNO";
    case DT_MIPS_SYMTABNO: return "MIPS_SYMTABNO";
    case DT_MIPS_UNREFEXTNO: return "MIPS_UNREFEXTNO";
    case DT_MIPS_GOTSYM: return "MIPS_GOTSYM";
    case DT_MIPS_HIPAGENO: return "MIPS_HIPAGENO";
    case DT_MIPS_RLD_MAP: return "MIPS_RLD_MAP";
    case DT_MIPS_DELTA_CLASS: return "MIPS_DELTA_CLASS";
    case DT_MIPS_DELTA_CLASS_NO: return "MIPS_DELTA_CLASS_NO";
    case DT_MIPS_DELTA_INSTANCE: return "MIPS_DELTA_INSTANCE";
    case DT_MIPS_DELTA_INSTANCE_NO: return "MIPS_DELTA_INSTANCE_NO";
    case DT_MIPS_DELTA_RELOC: return "MIPS_DELTA_RELOC";
    case DT_MIPS_DELTA_RELOC_NO: return "MIPS_DELTA_RELOC_NO";
    case DT_MIPS_DELTA_SYM: return "MIPS_DELTA_SYM";
    case DT_MIPS_DELTA_SYM_NO: return "MIPS_DELTA_SYM_NO";
    case DT_MIPS_DELTA_CLASSSYM: return "MIPS_DELTA_CLASSSYM";
    case DT_MIPS_DELTA_CLASSSYM_NO: return "MIPS_DELTA_CLASSSYM_NO";
    case DT_MIPS_CXX_FLAGS: return "MIPS_CXX_FLAGS";
    case DT_MIPS_PIXIE_INIT: return "MIPS_PIXIE_INIT";
    case DT_MIPS_SYMBOL_LIB: return "MIPS_SYMBOL_LIB";
    case DT_MIPS_LOCALPAGE_GOTIDX: return "MIPS_LOCALPAGE_GOTIDX";
    case DT_MIPS_LOCAL_GOTIDX: return "MIPS_LOCAL_GOTIDX";
    case DT_MIPS_HIDDEN_GOTIDX: return "MIPS_HIDDEN_GOTIDX";
    case DT_MIPS_PROTECTED_GOTIDX: return "MIPS_PROTECTED_GOTIDX";
    case DT_MIPS_OPTIONS: return "MIPS_OPTIONS";
    case DT_MIPS_INTERFACE: return "MIPS_INTERFACE";
    case DT_MIPS_DYNSTR_ALIGN: return "MIPS_DYNSTR_ALIGN";
    case DT_MIPS_INTERFACE_SIZE: return "MIPS_INTERFACE_SIZE";
    case DT_MIPS_RLD_TEXT_RESOLVE_ADDR: return "MIPS_RLD_TEXT_RESOLVE_ADDR";
    case DT_MIPS_PERF_SUFFIX: return "MIPS_PERF_SUFFIX";
    case DT_MIPS_COMPACT_SIZE: return "MIPS_COMPACT_SIZE";
    case DT_MIPS_GP_VALUE: return "MIPS_GP_VALUE";
    case DT_MIPS_AUX_DYNAMIC: return "MIPS_AUX_DYNAMIC";
    default:
      return NULL;
    }
}
*/

static const char *get_sparc64_dynamic_type (unsigned long type)
{
    switch (type)
    {
    case DT_SPARC_REGISTER: return "SPARC_REGISTER";
    default:
        return NULL;
    }
}

static const char *get_ppc64_dynamic_type (unsigned long type)
{
    switch (type)
    {
    case DT_PPC64_GLINK: return "PPC64_GLINK";
    case DT_PPC64_OPD:   return "PPC64_OPD";
    case DT_PPC64_OPDSZ: return "PPC64_OPDSZ";
    default:
        return NULL;
    }
}

/*static const char *get_parisc_dynamic_type (unsigned long type)
{
  switch (type)
    {
    case DT_HP_LOAD_MAP:   return "HP_LOAD_MAP";
    case DT_HP_DLD_FLAGS:   return "HP_DLD_FLAGS";
    case DT_HP_DLD_HOOK:   return "HP_DLD_HOOK";
    case DT_HP_UX10_INIT:   return "HP_UX10_INIT";
    case DT_HP_UX10_INITSZ:   return "HP_UX10_INITSZ";
    case DT_HP_PREINIT:     return "HP_PREINIT";
    case DT_HP_PREINITSZ:   return "HP_PREINITSZ";
    case DT_HP_NEEDED:     return "HP_NEEDED";
    case DT_HP_TIME_STAMP: return "HP_TIME_STAMP";
    case DT_HP_CHECKSUM:   return "HP_CHECKSUM";
    case DT_HP_GST_SIZE:   return "HP_GST_SIZE";
    case DT_HP_GST_VERSION:   return "HP_GST_VERSION";
    case DT_HP_GST_HASHVAL:   return "HP_GST_HASHVAL";
    default:
      return NULL;
    }
}
*/

static const char *get_dynamic_type(unsigned long type)
{
    static char buff [32];

    switch (type)
    {
    case DT_NULL:  return "NULL";
    case DT_NEEDED:  return "NEEDED";
    case DT_PLTRELSZ:   return "PLTRELSZ";
    case DT_PLTGOT:  return "PLTGOT";
    case DT_HASH:  return "HASH";
    case DT_STRTAB:  return "STRTAB";
    case DT_SYMTAB:  return "SYMTAB";
    case DT_RELA:  return "RELA";
    case DT_RELASZ:  return "RELASZ";
    case DT_RELAENT:  return "RELAENT";
    case DT_STRSZ:   return "STRSZ";
    case DT_SYMENT:  return "SYMENT";
    case DT_INIT:  return "INIT";
    case DT_FINI:  return "FINI";
    case DT_SONAME:  return "SONAME";
    case DT_RPATH:   return "RPATH";
    case DT_SYMBOLIC:   return "SYMBOLIC";
    case DT_REL:  return "REL";
    case DT_RELSZ:   return "RELSZ";
    case DT_RELENT:  return "RELENT";
    case DT_PLTREL:  return "PLTREL";
    case DT_DEBUG:   return "DEBUG";
    case DT_TEXTREL:  return "TEXTREL";
    case DT_JMPREL:  return "JMPREL";
    case DT_BIND_NOW:   return "BIND_NOW";
    case DT_INIT_ARRAY: return "INIT_ARRAY";
    case DT_FINI_ARRAY: return "FINI_ARRAY";
    case DT_INIT_ARRAYSZ: return "INIT_ARRAYSZ";
    case DT_FINI_ARRAYSZ: return "FINI_ARRAYSZ";
    case DT_RUNPATH:    return "RUNPATH";
    case DT_FLAGS:      return "FLAGS";

    case DT_PREINIT_ARRAY: return "PREINIT_ARRAY";
    case DT_PREINIT_ARRAYSZ: return "PREINIT_ARRAYSZ";

    case DT_CHECKSUM:   return "CHECKSUM";
    case DT_PLTPADSZ:   return "PLTPADSZ";
    case DT_MOVEENT:  return "MOVEENT";
    case DT_MOVESZ:  return "MOVESZ";
    case DT_FEATURE:  return "FEATURE";
    case DT_POSFLAG_1:  return "POSFLAG_1";
    case DT_SYMINSZ:  return "SYMINSZ";
    case DT_SYMINENT:   return "SYMINENT"; /* aka VALRNGHI */

    case DT_ADDRRNGLO:  return "ADDRRNGLO";
    case DT_CONFIG:  return "CONFIG";
    case DT_DEPAUDIT:   return "DEPAUDIT";
    case DT_AUDIT:   return "AUDIT";
    case DT_PLTPAD:  return "PLTPAD";
    case DT_MOVETAB:  return "MOVETAB";
    case DT_SYMINFO:  return "SYMINFO"; /* aka ADDRRNGHI */

    case DT_VERSYM:  return "VERSYM";

    case DT_RELACOUNT:  return "RELACOUNT";
    case DT_RELCOUNT:   return "RELCOUNT";
    case DT_FLAGS_1:  return "FLAGS_1";
    case DT_VERDEF:  return "VERDEF";
    case DT_VERDEFNUM:  return "VERDEFNUM";
    case DT_VERNEED:  return "VERNEED";
    case DT_VERNEEDNUM:  return "VERNEEDNUM";

    case DT_AUXILIARY:  return "AUXILIARY";
    case DT_USED:  return "USED";
    case DT_FILTER:  return "FILTER";

    case DT_GNU_PRELINKED: return "GNU_PRELINKED";
    case DT_GNU_CONFLICT: return "GNU_CONFLICT";
    case DT_GNU_CONFLICTSZ: return "GNU_CONFLICTSZ";
    case DT_GNU_LIBLIST: return "GNU_LIBLIST";
    case DT_GNU_LIBLISTSZ: return "GNU_LIBLISTSZ";

    default:
        if ((type >= DT_LOPROC) && (type <= DT_HIPROC))
        {
            const char * result;

            switch (elf_header.e_machine)
            {
                /*     case EM_MIPS:
       case EM_MIPS_RS3_LE:
         result = get_mips_dynamic_type (type);
         break;*/
            case EM_SPARCV9:
                result = get_sparc64_dynamic_type (type);
                break;
            case EM_PPC64:
                result = get_ppc64_dynamic_type (type);
                break;
            default:
                result = NULL;
                break;
            }

            if (result != NULL)
                return result;

            sprintf (buff, "Processor Specific: %lx", type);
        }
        else if ((type >= DT_LOOS) && (type <= DT_HIOS))
        {
            const char * result;

            switch (elf_header.e_machine)
            {
                /*     case EM_PARISC:
         result = get_parisc_dynamic_type (type);
         break;*/
            default:
                result = NULL;
                break;
            }

            if (result != NULL)
                return result;

            sprintf (buff, "Operating System specific: %lx", type);
        }
        else
            sprintf (buff, "<unknown>: %lx", type);

        return buff;
    }
}



static int get_file_header(FILE * file)
{
    /* Read in the identity array.  */
    if (fread (elf_header.e_ident, EI_NIDENT, 1, file) != 1)
        return 0;

    /* Determine how to read the rest of the header.  */
    switch (elf_header.e_ident [EI_DATA])
    {
    default: /* fall through */
    case ELFDATANONE: /* fall through */
    case ELFDATA2LSB: byte_get = byte_get_little_endian; break;
    case ELFDATA2MSB: byte_get = byte_get_big_endian; break;
    }

    /* For now we only support 32 bit and 64 bit ELF files.  */
    is_32bit_elf = (elf_header.e_ident [EI_CLASS] != ELFCLASS64);

    /* Read in the rest of the header.  */
    if (is_32bit_elf)
    {
        Elf32_External_Ehdr ehdr32;

        if (fread (ehdr32.e_type, sizeof (ehdr32) - EI_NIDENT, 1, file) != 1)
            return 0;

        elf_header.e_type      = BYTE_GET (ehdr32.e_type);
        elf_header.e_machine   = BYTE_GET (ehdr32.e_machine);
        elf_header.e_version   = BYTE_GET (ehdr32.e_version);
        elf_header.e_entry     = BYTE_GET (ehdr32.e_entry);
        elf_header.e_phoff     = BYTE_GET (ehdr32.e_phoff);
        elf_header.e_shoff     = BYTE_GET (ehdr32.e_shoff);
        elf_header.e_flags     = BYTE_GET (ehdr32.e_flags);
        elf_header.e_ehsize    = BYTE_GET (ehdr32.e_ehsize);
        elf_header.e_phentsize = BYTE_GET (ehdr32.e_phentsize);
        elf_header.e_phnum     = BYTE_GET (ehdr32.e_phnum);
        elf_header.e_shentsize = BYTE_GET (ehdr32.e_shentsize);
        elf_header.e_shnum     = BYTE_GET (ehdr32.e_shnum);
        elf_header.e_shstrndx  = BYTE_GET (ehdr32.e_shstrndx);
    }
    else
    {
        Elf64_External_Ehdr ehdr64;

        /* If we have been compiled with sizeof (bfd_vma) == 4, then
    we will not be able to cope with the 64bit data found in
    64 ELF files.  Detect this now and abort before we start
    overwritting things.  */
        if (sizeof (bfd_vma) < 8)
        {

            QMessageBox::warning(global_parent, "readELF warning", QString("This instance of readELF has been built without support for a\
                                                                           64 bit data type and so it cannot read 64 bit ELF files"),
                                 QMessageBox::Ok, QMessageBox::NoButton);

            /*   printf("This instance of readelf has been built without support for a\n\
     64 bit data type and so it cannot read 64 bit ELF files.\n");
 */
            return 0;
        }

        if (fread (ehdr64.e_type, sizeof (ehdr64) - EI_NIDENT, 1, file) != 1)
            return 0;

        elf_header.e_type      = BYTE_GET (ehdr64.e_type);
        elf_header.e_machine   = BYTE_GET (ehdr64.e_machine);
        elf_header.e_version   = BYTE_GET (ehdr64.e_version);
        elf_header.e_entry     = BYTE_GET8 (ehdr64.e_entry);
        elf_header.e_phoff     = BYTE_GET8 (ehdr64.e_phoff);
        elf_header.e_shoff     = BYTE_GET8 (ehdr64.e_shoff);
        elf_header.e_flags     = BYTE_GET (ehdr64.e_flags);
        elf_header.e_ehsize    = BYTE_GET (ehdr64.e_ehsize);
        elf_header.e_phentsize = BYTE_GET (ehdr64.e_phentsize);
        elf_header.e_phnum     = BYTE_GET (ehdr64.e_phnum);
        elf_header.e_shentsize = BYTE_GET (ehdr64.e_shentsize);
        elf_header.e_shnum     = BYTE_GET (ehdr64.e_shnum);
        elf_header.e_shstrndx  = BYTE_GET (ehdr64.e_shstrndx);
    }

    if (elf_header.e_shoff)
    {
        /* There may be some extensions in the first section header.  Don't
    bomb if we can't read it.  */
        if (is_32bit_elf)
            get_32bit_section_headers (file, 1);
        else
            get_64bit_section_headers (file, 1);
    }

    return 1;
}






static PTR get_data (PTR var, FILE *file, long offset, size_t size,const char *reason)
{
    PTR mvar;

    if (size == 0)
        return NULL;

    if (fseek (file, offset, SEEK_SET))
    {
        QMessageBox::warning(global_parent, "readELF warning", QString("Unable to seek to %1 for %2").arg(offset).arg(reason),
                             QMessageBox::Ok, QMessageBox::NoButton);

        //printf("Unable to seek to %x for %s\n", offset, reason);
        return NULL;
    }

    mvar = var;
    if (mvar == NULL)
    {
        mvar = (PTR) malloc (size);

        if (mvar == NULL)
        {
            QMessageBox::warning(global_parent, "readELF warning", QString("Out of memory allocating %1 bytes for %2").arg(size).arg(reason),
                                 QMessageBox::Ok, QMessageBox::NoButton);

            //   printf("Out of memory allocating %d bytes for %s\n",
            //     size, reason);
            return NULL;
        }
    }

    if (fread (mvar, size, 1, file) != 1)
    {
        /*      QMessageBox::warning(global_parent, "readELF warning", QString("Unable to read in %1 bytes of %2").arg(size).arg(reason),
                           QMessageBox::Ok, QMessageBox::NoButton);
*/
        if (mvar != var)
            free (mvar);
        return NULL;
    }

    return mvar;
}


