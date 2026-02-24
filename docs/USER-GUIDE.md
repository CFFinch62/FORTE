# FORTE User Guide

Welcome to **FORTE** (FORtran Teaching Environment), a beginner-friendly IDE for learning and writing Fortran. This guide covers installation, the interface, and all major features.

---

## 1. Installation and Running

### Prerequisites

- **Python 3.10+** — check with `python3 --version`
- **A Fortran compiler** — GNU Fortran (`gfortran`) is recommended:

  | Platform | Command |
  |----------|---------|
  | Debian / Ubuntu | `sudo apt install gfortran` |
  | Fedora / RHEL | `sudo dnf install gcc-gfortran` |
  | macOS (Homebrew) | `brew install gcc` |
  | Windows | Install [MSYS2](https://www.msys2.org/) then `pacman -S mingw-w64-x86_64-gcc-fortran` |

### Starting FORTE

1. Open a terminal in the FORTE project directory.
2. Run the launch script:
   ```bash
   ./run.sh
   ```
   The script automatically creates a Python virtual environment, installs all dependencies, and starts the application. No manual setup is required.

### Command-Line Options

```bash
python3 -m forte.main           # launch normally
python3 -m forte.main --version # print version and exit
```

---

## 2. Interface Overview

The main window is divided into three panels:

1. **File Browser (Left)** — navigate directories; shows only Fortran source files.
2. **Code Editor (Center/Top)** — multi-tab editor with syntax highlighting and line numbers.
3. **Terminal (Bottom)** — a full interactive shell (bash) for running programs and system commands.

### Menu Bar

| Menu | Key actions |
|------|-------------|
| **File** | New · Open · Save · Save As · Exit |
| **Edit** | Find/Replace · Preferences |
| **View** | Show/Hide File Browser · Show/Hide Terminal |
| **Build** | Compile & Run · Compile Only · Run Last Build · Clean · Clear Terminal · Restart Terminal · Interrupt |
| **Help** | About FORTE |

---

## 3. Code Editor

- **Syntax Highlighting** — Fortran keywords, intrinsic functions, string literals, comments (`!`), and preprocessor directives are color-coded. Matching is case-insensitive, consistent with Fortran's own rules.
- **Line Numbers** — displayed in the left gutter; updates as you type.
- **Multi-Tab Editing** — open any number of files simultaneously. Tabs show a `*` indicator when a file has unsaved changes.
- **Auto-Indent** — pressing Enter inside a block preserves the current indentation level.
- **Find & Replace** — open with `Ctrl+F`. Supports case-sensitive toggle, wrap-around, Replace, and Replace All.

---

## 4. Build System

FORTE uses a non-blocking compile pipeline built on `gfortran` (or your preferred compiler). Output streams live into the terminal panel as it arrives.

### Workflow

1. Open or create a `.f90` file in the editor.
2. Press **`Ctrl+R`** (Compile & Run) or choose **Build → Compile & Run**.
   - FORTE saves the file automatically before compiling.
   - The compiler command and its output appear in the terminal.
   - On success, the binary runs immediately in the terminal.
3. Use **`Ctrl+B`** (Compile Only) to check for errors without running.
4. Use **Build → Run Last Build** to re-run the most recently compiled binary without recompiling.
5. Use **Build → Clean Build Output** to delete the compiled binary.

### Compiler Flags

The default flags are configured in **Edit → Preferences → Build**. The `Extra flags` field accepts any flags supported by your compiler (e.g., `-Wall -O2 -std=f2008`).

### Output Directory

By default the binary is placed in the same directory as the source file (setting `"."`). Change this in **Preferences → Build → Output directory**.

---

## 5. Integrated Terminal

The bottom panel is a full PTY shell — the same as opening a terminal window on your desktop.

- **Interactive** — type any shell command and press Enter.
- **Compiler output** — build messages from FORTE appear here in colour (blue headers, green/red status).
- **`Ctrl+C`** — sends an interrupt signal to the running process.
- **Restart Terminal** — **Build → Restart Terminal** kills and relaunches the shell.
- **Clear Terminal** — **Build → Clear Terminal** wipes the visible output.

---

## 6. File Browser

- **Navigation** — double-click folders to expand/collapse.
- **Open files** — double-click a Fortran file to open it in a new editor tab.
- **Filtered view** — only files with Fortran extensions are shown (`.f90`, `.f95`, `.f03`, `.f08`, `.f`, `.for`, `.f77`, `.ftn`).
- **Context menu** — right-click to create a new file, create a new folder, rename, or delete.
- **Root directory** — set the starting directory in **Edit → Preferences → Editor → Default directory**.

---

## 7. Preferences

Open with **Edit → Preferences** (`Ctrl+,`).

### Editor Tab
| Setting | Description |
|---------|-------------|
| Font family | Monospace font used in the editor and terminal |
| Font size | Point size (default: 12) |
| Tab width | Spaces per indent level (default: 3) |
| Show line numbers | Toggle the gutter on/off |
| Default directory | Directory the file browser opens on startup |

### Build Tab
| Setting | Description |
|---------|-------------|
| Compiler | Select from auto-detected compilers; use custom path if needed |
| Extra flags | Additional compiler flags (e.g., `-Wall -O2`) |
| Output directory | Where to place compiled binaries (`.` = same as source) |

### Appearance Tab
| Setting | Description |
|---------|-------------|
| Theme | **Dark** (default) or **Light** |

---

## 8. Example Programs

FORTE ships with eight ready-to-run programs in the `examples/` folder. Open any one in the file browser, then press `Ctrl+R` to compile and run it.

| File | Topic |
|------|-------|
| `hello_world.f90` | Classic first program — PRINT * |
| `variables.f90` | INTEGER, REAL, DOUBLE PRECISION, CHARACTER, LOGICAL |
| `loops.f90` | Counted DO loop, DO WHILE, EXIT / CYCLE |
| `conditionals.f90` | IF / ELSE IF / ELSE, logical operators, SELECT CASE |
| `subroutines.f90` | SUBROUTINE with INTENT, FUNCTION, recursive factorial |
| `arrays.f90` | 1-D / 2-D arrays, constructors, SUM / MAXVAL / MINVAL |
| `io.f90` | Formatted PRINT, WRITE/READ, OPEN / CLOSE file I/O |
| `modules.f90` | MODULE / USE pattern, derived type Point2D |

---

## 9. Keyboard Shortcuts

| Action | Shortcut |
|--------|----------|
| New file | `Ctrl+N` |
| Open file | `Ctrl+O` |
| Save | `Ctrl+S` |
| Save As | `Ctrl+Shift+S` |
| Find / Replace | `Ctrl+F` |
| Preferences | `Ctrl+,` |
| Compile & Run | `Ctrl+R` |
| Compile Only | `Ctrl+B` |

---

## 10. Troubleshooting

**"No compilers found" in Preferences**
Ensure a Fortran compiler is installed and on your `PATH`. After installing, click **Refresh** in the Build preferences tab. If your compiler is in a non-standard location, enable **Use custom path** and browse to the binary.

**Build fails with "command not found"**
Open a terminal and run `which gfortran` to confirm it is on `PATH`. On macOS, `brew install gcc` installs `gfortran` in `/usr/local/bin` or `/opt/homebrew/bin` — make sure that directory is in your shell's `PATH`.

**Terminal shows no output after running**
Some programs buffer output. Add `FLUSH(6)` after `PRINT *` statements, or compile with `-fflushing` if using gfortran.

**Window layout not restored on reopening**
Layout is saved to `~/.config/forte/settings.json`. If the file is corrupt or missing, delete it and FORTE will recreate it with defaults on the next launch.

