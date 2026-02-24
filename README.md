# FORTE — FORtran Teaching Environment

![FORTE Screenshot](images/FORTE.png)

**FORTE** is a beginner-friendly Integrated Development Environment (IDE) designed specifically for students and new programmers learning **Fortran**. Built with Python and PyQt6, it provides a modern, accessible interface with a syntax-highlighted editor, an integrated terminal, and a one-click compile-and-run workflow.

## ✨ Features

*   **Syntax Highlighting:** Color-coded keywords, intrinsic functions, strings, comments, and preprocessor directives — all case-insensitive, matching Fortran's own rules.
*   **Multi-Tab Editor:** Open and edit multiple `.f90` files side-by-side with unsaved-change indicators.
*   **Integrated Terminal:** A full PTY shell (bash) in the lower panel for running programs and interacting with the system.
*   **One-Click Build:** Compile & Run (`Ctrl+R`) or Compile Only (`Ctrl+B`) — gfortran output streams live into the terminal.
*   **Project File Browser:** Left-panel file tree filtered to Fortran source files (`.f90`, `.f95`, `.f03`, `.f08`, `.f`, `.for`, `.f77`, `.ftn`).
*   **Find & Replace:** Full find/replace dialog with case-sensitive toggle, wrap-around, and Replace All.
*   **Dark & Light Themes:** Switch themes instantly from Preferences; all colors are theme-driven with no hardcoded values.
*   **Multi-Compiler Support:** Auto-detects installed Fortran compilers (gfortran, ifort, ifx, flang, lfortran, nagfor, nvfortran) with easy switching via Preferences.
*   **Bundled Examples:** Eight ready-to-run example programs covering hello world, variables, loops, conditionals, subroutines, arrays, I/O, and modules.

## 🚀 Quick Start

### Prerequisites

*   **Linux, macOS, or Windows**
*   **Python 3.10+**
*   **A Fortran compiler** — [GNU Fortran](https://gcc.gnu.org/fortran/) is recommended:
    ```bash
    # Debian / Ubuntu
    sudo apt install gfortran

    # macOS (Homebrew)
    brew install gcc

    # Fedora / RHEL
    sudo dnf install gcc-gfortran
    ```

### Installation & Running

1.  **Clone the repository:**
    ```bash
    git clone https://github.com/fragillidae/FORTE.git
    cd FORTE
    ```

2.  **Launch the application:**
    The included script automatically creates a virtual environment and installs all Python dependencies.
    ```bash
    ./run.sh
    ```

### Building a Standalone Executable

To produce a self-contained binary for your platform (no Python installation required on the target machine):

```bash
source venv/bin/activate
python3 build.py
```

The executable will be located in `dist/FORTE/`.

## ⌨️ Keyboard Shortcuts

| Action | Shortcut |
|--------|----------|
| New file | `Ctrl+N` |
| Open file | `Ctrl+O` |
| Save | `Ctrl+S` |
| Save As | `Ctrl+Shift+S` |
| Compile & Run | `Ctrl+R` |
| Compile Only | `Ctrl+B` |
| Find / Replace | `Ctrl+F` |
| Preferences | `Ctrl+,` |

## 📖 Documentation

*   [**User Guide**](docs/USER-GUIDE.md): Full usage instructions, interface overview, and workflow examples.

## 🛠️ Technology Stack

*   **GUI:** Python 3.10+, PyQt6
*   **Terminal Emulation:** `pyte`, `ptyprocess`
*   **Compiler Backend:** gfortran (default), with support for ifort, ifx, flang, lfortran, nagfor, and nvfortran
*   **Packaging:** PyInstaller 6

## 📜 License

MIT License — © 2026 Chuck Finch · Fragillidae Software

See [LICENSE](LICENSE) for full terms.

