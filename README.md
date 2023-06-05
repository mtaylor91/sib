# SIB: System Image Builder

SIB is a tool to drive system image builds via YAML specifications.

SIB specs are comprised of three top-level keys:

 - `name`: The name of the specification.
 - `steps`: A list of build steps.
 - `contexts`: A list of contexts used in the build.

## Steps

Steps represent distinct steps of the build.

Various types of steps are supported.

### Command Steps

Command steps consist of either a single command or a list of commands.

Shell interpolation is not supported, but can be achieved trivially by invoking
a shell such as `bash` with the `-c` option.

### File Steps

File steps can be used to write a string to a file.

### Download Steps

Download steps can be used to download a file over the network.

### Enter/Leave Context Steps

Entering and leaving enters or leaves a context, and is described in more detail below.

The order of leaves must be the reverse of the order of enters to preserve appropriate
nesting.

## Contexts

Contexts are bounded and nested "contexts" in which steps can run.

Currently there are three supported context types.

### Command Contexts

Command contexts run a list of commands when the context is entered and another
list of commands when the context is left.

### Directory Contexts

Directory contexts manipulate the working directory of steps being run.

On entering a directory context the working directory is effectively changed to the
specified directory and is restored to the previous directory (either the starting
directory or a previous context directory) on leave.

### Chroot Contexts

TODO: describe the effects of chroot contexts in more detail.
