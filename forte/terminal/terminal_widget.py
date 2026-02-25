# forte/terminal/terminal_widget.py - Terminal display widget (pyte + PTY)

import os

import pyte
from PyQt6.QtWidgets import QWidget
from PyQt6.QtCore import Qt, QTimer
from PyQt6.QtGui import (
    QPainter, QFont, QColor, QFontMetrics, QKeyEvent, QBrush, QMouseEvent,
)

from forte.terminal.pty_process import PTYProcess
from forte.config.settings import Settings


class TerminalWidget(QWidget):
    """PTY-backed terminal panel using pyte for VT100 emulation."""

    def __init__(self, parent=None):
        super().__init__(parent)
        self.settings = Settings()

        # Terminal dimensions (updated on resize)
        self.cols = 80
        self.rows = 24

        # pyte screen + stream
        self.screen = pyte.Screen(self.cols, self.rows)
        self.stream = pyte.Stream(self.screen)

        # PTY process — always starts the system shell
        shell = "/bin/bash"
        self.pty = PTYProcess(command=[shell])
        self.pty.data_received.connect(self.on_data_received)
        self.pty.process_exited.connect(self.on_process_exited)

        # Font
        self.setup_font()

        # Blinking cursor
        self.cursor_visible = True
        self.cursor_timer = QTimer(self)
        self.cursor_timer.timeout.connect(self.toggle_cursor)
        self.cursor_timer.start(500)

        self.setFocusPolicy(Qt.FocusPolicy.StrongFocus)
        self.setAttribute(Qt.WidgetAttribute.WA_InputMethodEnabled, True)

        # Launch the shell
        self.pty.start()

    # ------------------------------------------------------------------
    # Setup
    # ------------------------------------------------------------------

    def setup_font(self) -> None:
        font_family = self.settings.get("editor", "font_family") or "Monospace"
        font_size = self.settings.get("editor", "font_size") or 14

        self.font = QFont(font_family, font_size)
        self.font.setStyleHint(QFont.StyleHint.Monospace)
        self.fm = QFontMetrics(self.font)

        self.char_width = self.fm.horizontalAdvance("W")
        self.char_height = self.fm.height()

        self.resize_terminal()

    # ------------------------------------------------------------------
    # Resize
    # ------------------------------------------------------------------

    def resizeEvent(self, event) -> None:
        self.resize_terminal()
        super().resizeEvent(event)

    def resize_terminal(self) -> None:
        new_cols = max(1, self.width() // self.char_width)
        new_rows = max(1, self.height() // self.char_height)

        if new_cols != self.cols or new_rows != self.rows:
            self.cols = new_cols
            self.rows = new_rows
            self.screen.resize(self.rows, self.cols)
            self.pty.resize(self.rows, self.cols)
            self.update()

    # ------------------------------------------------------------------
    # PTY callbacks
    # ------------------------------------------------------------------

    def on_data_received(self, data: bytes) -> None:
        try:
            self.stream.feed(data.decode("utf-8", errors="replace"))
            self.update()
        except Exception:
            pass

    def on_process_exited(self, exit_code: int) -> None:
        self.stream.feed(f"\r\n[Process exited with code {exit_code}]\r\n")
        self.update()

    def write(self, data: bytes) -> None:
        """Send *data* to the PTY (used by BuildManager to run programs)."""
        self.pty.write(data)

    # ------------------------------------------------------------------
    # Keyboard input
    # ------------------------------------------------------------------

    def keyPressEvent(self, event: QKeyEvent) -> None:
        key = event.key()
        text = event.text()
        mods = event.modifiers()

        if key == Qt.Key.Key_Return:
            self.write(b"\r")
        elif key == Qt.Key.Key_Backspace:
            self.write(b"\x7f")
        elif key == Qt.Key.Key_Tab:
            self.write(b"\t")
        elif key == Qt.Key.Key_Up:
            self.write(b"\x1b[A")
        elif key == Qt.Key.Key_Down:
            self.write(b"\x1b[B")
        elif key == Qt.Key.Key_Right:
            self.write(b"\x1b[C")
        elif key == Qt.Key.Key_Left:
            self.write(b"\x1b[D")
        elif mods & Qt.KeyboardModifier.ControlModifier and key == Qt.Key.Key_C:
            self.write(b"\x03")   # SIGINT
        elif mods & Qt.KeyboardModifier.ControlModifier and key == Qt.Key.Key_D:
            self.write(b"\x04")   # EOF
        elif text:
            self.write(text.encode("utf-8"))

    # ------------------------------------------------------------------
    # Painting
    # ------------------------------------------------------------------

    def paintEvent(self, event) -> None:
        painter = QPainter(self)
        painter.setFont(self.font)

        bg_color = QColor("#0f0f0f")
        fg_color = QColor("#cccccc")
        painter.fillRect(self.rect(), bg_color)

        for y in range(self.rows):
            row_chars = self.screen.buffer[y]
            for x in range(self.cols):
                char_data = row_chars[x]
                painter.setPen(fg_color)
                painter.drawText(
                    x * self.char_width,
                    y * self.char_height + self.fm.ascent(),
                    char_data.data,
                )

        # Blinking block cursor
        if self.cursor_visible and self.pty.running:
            cx = self.screen.cursor.x
            cy = self.screen.cursor.y
            painter.setPen(Qt.PenStyle.NoPen)
            painter.setBrush(QBrush(QColor(200, 200, 200, 100)))
            painter.drawRect(
                cx * self.char_width,
                cy * self.char_height,
                self.char_width,
                self.char_height,
            )

    def toggle_cursor(self) -> None:
        self.cursor_visible = not self.cursor_visible
        cx = self.screen.cursor.x
        cy = self.screen.cursor.y
        self.update(
            cx * self.char_width, cy * self.char_height,
            self.char_width, self.char_height,
        )

    # ------------------------------------------------------------------
    # Public actions (wired to menu in Phase 7)
    # ------------------------------------------------------------------

    def restart(self) -> None:
        """Kill the current shell and start a fresh one."""
        self.pty.terminate_process()
        self.pty.wait()
        self.screen.reset()

        shell = "/bin/bash"
        self.pty = PTYProcess(command=[shell])
        self.pty.data_received.connect(self.on_data_received)
        self.pty.process_exited.connect(self.on_process_exited)
        self.pty.start()
        self.pty.resize(self.rows, self.cols)
        self.update()

    def clear(self) -> None:
        """Clear the terminal screen buffer."""
        self.screen.reset()
        self.update()

    def interrupt(self) -> None:
        """Send Ctrl+C to the foreground process."""
        self.write(b"\x03")

    # ------------------------------------------------------------------
    # Mouse / focus
    # ------------------------------------------------------------------

    def mousePressEvent(self, event: QMouseEvent) -> None:
        self.setFocus(Qt.FocusReason.MouseFocusReason)
        event.accept()

    def focusInEvent(self, event) -> None:
        self.cursor_visible = True
        self.update()
        super().focusInEvent(event)

    def focusOutEvent(self, event) -> None:
        super().focusOutEvent(event)

