#! /usr/bin/env python3

"""
Generate asli script to generate CPP code from ASL

Typical usage:

    mkdir genc
    bin/asl2cpp.py --output-dir=genc --basename=sim --intermediates=genc/log --line-info --num-c-files=8 > genc/asl2cpp.prj
    asli --batchmode --nobanner --project=genc/asl2cpp.prj --configuration=imports.json --configuration=exports.json spec.asl
"""

import argparse
import string
import sys

base_script = """
// Autogenerated script to generate CPP code from ASL
//
// Generated by command: {command}

// Discard any code not reachable from the list of exported functions.
// This is done early because it can remove code that we cannot compile.
//
// The list of exports is loaded using the "--configuration=foo.json" flag
// and an example json file that exports two functions called "Reset" and "Step"
// is as follows.
//
//     {{
//         "__comment": [
//             "Export the interface required by the simulator"
//         ],
//         "exports": [
//             "Reset",
//             "Step"
//         ]
//     }}
//
// The "__comment" part is optional and is ignored.
//
// Multiple configuration files can be loaded if you want to group the set of
// exports into multiple logical interfaces.
//
:filter_reachable_from exports

// A series of 'desugaring' passes eliminate features that complicate later transformations.
// Later transformations will fail if these features have not been removed.

// Eliminate 'typedef'
:xform_named_type

// Eliminate bit,int arithmetic operations like "'000' + 3"
:xform_desugar

// Eliminate bit-tuples like "[x,y] = z;" and "x[7:0, 15:8]";
:xform_bittuples

// Convert all bit-slice operations to use the +: syntax.
// e.g., "x[7:0]" --> "x[0 +: 8]"
:xform_lower

// Convert use of getter/setter syntax to function calls.
// e.g., "Mem[a, sz] = x" --> "Mem_set(a, sz, x)"
:xform_getset

{track_valid}

// Perform constant propagation without unrolling loops
// This helps identify potential targets for the monomorphization pass.
:xform_constprop --unroll

// Create specialized versions of every bitwidth-polymorphic function
// and change all function calls to use the appropriate specialized
// version.
// (Note that this performs an additional round of constant propagation.)
:xform_monomorphize

// Discard any code not reachable from the list of exported functions.
// This step is repeated because it deletes any bitwidth-polymorphic functions
// that have been completely replaced by specialized versions of the functions.
:filter_reachable_from exports

// todo: explain why this needs to be repeated
:xform_monomorphize

// Perform a further round of simplifying passes.
// This is done after performing monomorphization and constant propagation
// because they work better in code where bitwidths are constants
// or they make constant propagation less precise.

// Change any function that returns multiple results to return a
// record with multiple fields and change all calls to that function.
// (This makes constant propagation less precise because constant propagation
// is not field sensitive.)
:xform_tuples

// Convert use of getter/setter syntax to function calls.
// e.g., "Mem[a, sz] = x" --> "Mem_set(a, sz, x)"
// (This is repeated because xform_tuples can expose additional
// getter/setter calls.)
:xform_getset

// todo: explain why this needs to be repeated
:xform_bittuples

// Convert bitslice operations like "x[i] = '1';" to a combination
// of AND/OR and shift operations like "x = x OR (1 << i);"
// This works better after constant propagation/monomorphization.
:xform_bitslices --notransform

// Any case statement that does not correspond to what the C language
// supports is converted to an if statement.
// This works better after constant propagation/monomorphization
// because that can eliminate/simplify guards on the clauses of the case statement.
:xform_case

// Eliminate slices of integers by first converting the integer to a bitvector.
// e.g., if "x : integer", then "x[1 +: 8]" to "cvt_int_bits(x, 9)[1 +: 8]"
:xform_int_bitslices

{wrap_variables}

// To let the generated code call your own functions, you need to declare
// the type of an ASL function with a matching type and provide a configuration
// file containing a list of these external functions.
// For example, if you want to track reads and writes to memory, you might
// use this import file (loaded using the --configuration flag)
//
//     {{
//         "__comment": [
//             "Import memory trace functions"
//         ],
//         "imports": [
//             "TraceMemRead",
//             "TraceMemWrite"
//         ]
//     }}
//
// If you have defined the behavior of these functions in ASL (e.g., for use in
// the ASL interpreter), you need to delete the ASL definitions of these
// functions.
:filter_unlisted_functions imports

// Remove global variables corresponding to thread-local variables
// (Thread local variables are listed in a configuration file like this
//
//     {{
//         "thread_local_state": [
//             "GPR",
//             "RFLAGS"
//         ]
//     }}
:filter_listed_variables thread_local_state

// Deleting the ASL definitions of any functions on the import list may
// result in additional dead code (i.e., functions that are only used by
// those functions) so delete any unreachable functions
:filter_reachable_from exports

// Check that all definitions are bitwidth-monomorphic and report a useful
// error message if they are not.
// The code generator will produce an error message if it finds a call
// to a polymorphic functions but we can produce a much more useful error message
// if we scan for all polymorphic functions and organize the list of functions
// into a call tree so that you can see which functions are at the roots of
// the tree (and therefore are the ones that you need to fix).
:check_monomorphization --fatal --verbose

// Generate C code from the remaining definitions
//
// This produces multiple files that will be prefixed by the basename and saved
// into the output-dir directory.
//
// Compilation of all the functions can be parallelized by splitting the
// file containing the functions into smaller files (using --num-c-files=<N>)
// and compiling them in parallel.
//
// Optionally, the C code can use #line directives so that profiling, debuggers,
// etc. know what file/line in the ASL file produced each line of C code.
// Use --line-info or --no-line-info to control this.
//
// Optionally, references to thread-local processor state such as registers
// can be accessed via a pointer.
// (This can be useful when modelling multi-processor systems.)
:generate_cpp --output-dir={output_dir} --basename={basename} --num-c-files={num_c_files} {line_info} {thread_local}
""".strip()

def main() -> int:
    parser = argparse.ArgumentParser(
            prog = 'asl2cpp',
            description = __doc__,
            formatter_class=argparse.RawDescriptionHelpFormatter,
            )
    parser.add_argument("--intermediates", help="generate intermediate files with prefix", metavar="log_prefix")
    parser.add_argument("--output-dir", help="output directory for generated files", metavar="output_dir", default="")
    parser.add_argument("--basename", help="basename of generated C files", metavar="output_prefix", required=True)
    parser.add_argument("--num-c-files", help="write functions to N files", metavar="N", type=int, default=1)
    parser.add_argument("--line-info", help="insert line directives into C code", action=argparse.BooleanOptionalAction)
    parser.add_argument("--thread-local-pointer", help="name of pointer to thread-local processor state", metavar="varname", default=None)
    parser.add_argument("--instrument-unknown", help="instrument assignments of UNKNOWN", action=argparse.BooleanOptionalAction)
    parser.add_argument("--wrap-variables", help="wrap global variables into functions", action=argparse.BooleanOptionalAction)
    args = parser.parse_args()

    substitutions = {
        'command':     " ".join(sys.argv),
        'basename':    args.basename,
        'line_info':   "",
        'num_c_files': args.num_c_files,
        'output_dir':  args.output_dir,
        'track_valid': "",
        'wrap_variables': "",
    }
    if args.instrument_unknown: substitutions['track_valid'] = ":xform_valid track-valid"
    if args.wrap_variables: substitutions['wrap_variables'] = ":xform_wrap"
    if not args.line_info:
        substitutions['line_info'] = '--no-line-info'
    else:
        substitutions['line_info'] = '--line-info'
    if args.thread_local_pointer:
        thread_local = f"--thread-local-pointer={args.thread_local_pointer}"
        thread_local += f" --thread-local=thread_local_state"
    else:
        thread_local = ''
    substitutions['thread_local'] = thread_local

    script = base_script.format(**substitutions)

    if args.intermediates:
        lines_in = script.splitlines()
        lines_out = []
        i = 0
        for l in lines_in:
            if l.startswith(":"):
                command = l.split()[0][1:]
                lines_out.append(f":show --format=raw --output {args.intermediates}.{i:02}.{command}.asl")
                i = i + 1
            lines_out.append(l)

        script = "\n".join(lines_out)

    print(script)


if __name__ == "__main__":
    main()

################################################################
# End
################################################################