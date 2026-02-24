# forte/editor/highlighter.py - Fortran syntax highlighter

from __future__ import annotations

from PyQt6.QtGui import QSyntaxHighlighter, QTextCharFormat, QColor, QFont
from PyQt6.QtCore import QRegularExpression

from forte.config.themes import Theme, get_theme


class FortranHighlighter(QSyntaxHighlighter):
    """Syntax highlighter for Fortran 77/90/95/03/08 source code.

    Rules are applied in order; later rules override earlier ones on the same
    span of text.  Comments are added last so ``!`` always wins.
    """

    def __init__(self, document, theme: Theme | None = None):
        super().__init__(document)
        self.highlighting_rules: list[tuple] = []
        self._build_rules(theme or get_theme("dark"))

    def set_theme(self, theme: Theme) -> None:
        """Rebuild highlighting rules for *theme* and re-colour the document."""
        self._build_rules(theme)
        self.rehighlight()

    # ------------------------------------------------------------------
    # Rule construction
    # ------------------------------------------------------------------

    def _build_rules(self, theme: Theme) -> None:
        self.highlighting_rules.clear()
        ci = QRegularExpression.PatternOption.CaseInsensitiveOption

        # 1. Numbers — integer, real, exponential (e/d notation), kind suffix
        num_fmt = QTextCharFormat()
        num_fmt.setForeground(QColor(theme.number))
        self.highlighting_rules.append((
            QRegularExpression(
                r"\b[0-9]+(\.[0-9]*)?([eEdD][+-]?[0-9]+)?(_[a-zA-Z][a-zA-Z0-9_]*)?\b"
            ),
            num_fmt,
        ))

        # 2. Type-specifier keywords
        type_fmt = QTextCharFormat()
        type_fmt.setForeground(QColor(theme.type_keyword))
        for pattern in [
            r"\bDOUBLE\s+PRECISION\b",
            r"\bDOUBLE\s+COMPLEX\b",
            r"\bINTEGER\b", r"\bREAL\b", r"\bCOMPLEX\b",
            r"\bCHARACTER\b", r"\bLOGICAL\b",
            r"\bTYPE\b", r"\bCLASS\b", r"\bKIND\b",
        ]:
            self.highlighting_rules.append((QRegularExpression(pattern, ci), type_fmt))

        # 3. Control / program-structure keywords (bold)
        kw_fmt = QTextCharFormat()
        kw_fmt.setForeground(QColor(theme.keyword))
        kw_fmt.setFontWeight(QFont.Weight.Bold)
        for pattern in [
            r"\bBLOCK\s+DATA\b",
            r"\bDO\s+WHILE\b",
            r"\bEND\s+DO\b", r"\bEND\s+IF\b", r"\bEND\s+SELECT\b",
            r"\bEND\s+WHERE\b", r"\bEND\s+FORALL\b",
            r"\bELSE\s+IF\b",
            r"\bSELECT\s+CASE\b", r"\bSELECT\s+TYPE\b",
            r"\bSUBMODULE\b",
            r"\bPROGRAM\b", r"\bSUBROUTINE\b", r"\bFUNCTION\b",
            r"\bMODULE\b", r"\bCONTAINS\b",
            r"\bUSE\b", r"\bONLY\b",
            r"\bIMPLICIT\b", r"\bINTERFACE\b",
            r"\bDO\b", r"\bENDDO\b",
            r"\bIF\b", r"\bTHEN\b", r"\bELSE\b", r"\bENDIF\b",
            r"\bCASE\b", r"\bWHERE\b", r"\bELSEWHERE\b",
            r"\bFORALL\b",
            r"\bCYCLE\b", r"\bEXIT\b", r"\bRETURN\b",
            r"\bSTOP\b", r"\bGOTO\b", r"\bCONTINUE\b",
            r"\bCALL\b", r"\bEND\b", r"\bNONE\b",
        ]:
            self.highlighting_rules.append((QRegularExpression(pattern, ci), kw_fmt))

        # 4. Attribute keywords
        attr_fmt = QTextCharFormat()
        attr_fmt.setForeground(QColor(theme.type_keyword))
        for word in [
            "ALLOCATABLE", "POINTER", "TARGET", "INTENT",
            "DIMENSION", "PARAMETER", "SAVE", "EXTERNAL", "INTRINSIC",
            "OPTIONAL", "VALUE", "VOLATILE", "PURE", "ELEMENTAL", "RECURSIVE",
        ]:
            self.highlighting_rules.append((
                QRegularExpression(rf"\b{word}\b", ci), attr_fmt
            ))

        # 5. Intrinsic functions
        intr_fmt = QTextCharFormat()
        intr_fmt.setForeground(QColor(theme.intrinsic))
        for word in [
            "ABS", "SQRT", "SIN", "COS", "TAN",
            "ASIN", "ACOS", "ATAN", "ATAN2",
            "EXP", "LOG", "LOG10", "MOD", "MAX", "MIN",
            "INT", "REAL", "NINT", "FLOOR", "CEILING",
            "MODULO", "SIGN", "DIM", "AIMAG", "CONJG",
            "LEN", "LEN_TRIM", "TRIM", "ADJUSTL", "ADJUSTR",
            "INDEX", "SCAN", "VERIFY",
            "ALLOCATED", "ASSOCIATED", "PRESENT",
            "SIZE", "SHAPE", "LBOUND", "UBOUND",
            "RESHAPE", "TRANSPOSE", "MATMUL", "DOT_PRODUCT",
            "SUM", "PRODUCT", "COUNT", "ANY", "ALL",
            "MAXVAL", "MINVAL", "MAXLOC", "MINLOC",
            "WRITE", "READ", "PRINT", "OPEN", "CLOSE",
            "INQUIRE", "FLUSH", "REWIND", "BACKSPACE",
        ]:
            self.highlighting_rules.append((
                QRegularExpression(rf"\b{word}\b", ci), intr_fmt
            ))

        # 6. Preprocessor directives (#include, #define, …)
        pre_fmt = QTextCharFormat()
        pre_fmt.setForeground(QColor(theme.preprocessor))
        self.highlighting_rules.append((
            QRegularExpression(
                r"^\s*#(include|define|ifdef|ifndef|endif|if|else)\b.*", ci
            ),
            pre_fmt,
        ))

        # 7. String literals — single-quoted and double-quoted
        str_fmt = QTextCharFormat()
        str_fmt.setForeground(QColor(theme.string))
        self.highlighting_rules.append((QRegularExpression(r"'[^']*'"), str_fmt))
        self.highlighting_rules.append((QRegularExpression(r'"[^"]*"'), str_fmt))

        # 8. Comments — must be LAST so '!' overrides everything after it
        comment_fmt = QTextCharFormat()
        comment_fmt.setForeground(QColor(theme.comment))
        self.highlighting_rules.append((QRegularExpression(r"!.*"), comment_fmt))

    # ------------------------------------------------------------------
    # Qt override
    # ------------------------------------------------------------------

    def highlightBlock(self, text: str) -> None:
        for pattern, fmt in self.highlighting_rules:
            it = pattern.globalMatch(text)
            while it.hasNext():
                match = it.next()
                self.setFormat(match.capturedStart(), match.capturedLength(), fmt)

