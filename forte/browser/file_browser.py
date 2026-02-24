# forte/browser/file_browser.py - File browser panel

import os

from PyQt6.QtWidgets import (
    QWidget, QVBoxLayout, QTreeView, QMenu, QMessageBox, QInputDialog,
)
from PyQt6.QtCore import Qt, pyqtSignal, QDir, QSortFilterProxyModel
from PyQt6.QtGui import QAction, QFileSystemModel

from forte.config.settings import Settings


FORTRAN_EXTENSIONS = {
    ".f90", ".f95", ".f03", ".f08", ".f", ".for", ".f77", ".ftn",
}


class FortranFileFilterProxy(QSortFilterProxyModel):
    """Proxy model that optionally hides non-Fortran files."""

    def __init__(self, parent=None):
        super().__init__(parent)
        self.enabled = False

    def filterAcceptsRow(self, source_row, source_parent):
        if not self.enabled:
            return True

        model = self.sourceModel()
        index = model.index(source_row, 0, source_parent)

        # Always show directories so the tree stays navigable
        if model.isDir(index):
            return True

        _, ext = os.path.splitext(model.fileName(index))
        return ext.lower() in FORTRAN_EXTENSIONS


class FileBrowser(QWidget):
    """Left-panel file tree widget."""

    file_selected = pyqtSignal(str)   # Emits absolute path of chosen file

    def __init__(self, parent=None):
        super().__init__(parent)
        self.settings = Settings()

        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)

        # Filesystem model
        self.fs_model = QFileSystemModel()
        self.fs_model.setRootPath(QDir.rootPath())

        # Filter proxy
        self.proxy_model = FortranFileFilterProxy(self)
        self.proxy_model.setSourceModel(self.fs_model)

        # Apply fortran_filter setting
        self.proxy_model.enabled = bool(
            self.settings.get("browser", "fortran_filter")
        )

        # Tree view
        self.tree = QTreeView()
        self.tree.setModel(self.proxy_model)

        # Hide Size / Type / Date columns — Name only
        self.tree.setColumnHidden(1, True)
        self.tree.setColumnHidden(2, True)
        self.tree.setColumnHidden(3, True)
        self.tree.setHeaderHidden(True)

        self.tree.doubleClicked.connect(self._on_double_click)
        self.tree.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self.tree.customContextMenuRequested.connect(self._show_context_menu)

        layout.addWidget(self.tree)

        self._load_settings()

    # ------------------------------------------------------------------
    # Navigation
    # ------------------------------------------------------------------

    def _on_double_click(self, index):
        source_index = self.proxy_model.mapToSource(index)
        path = self.fs_model.filePath(source_index)
        if self.fs_model.isDir(source_index):
            self.tree.setRootIndex(index)
            # Persist the navigated-to directory
            self.settings.set("browser", "last_directory", path)
            self.settings.save()
        else:
            self.file_selected.emit(path)

    def navigate_to(self, path: str):
        """Programmatically set the root of the tree view."""
        src_index = self.fs_model.index(path)
        self.tree.setRootIndex(self.proxy_model.mapFromSource(src_index))

    # ------------------------------------------------------------------
    # Context menu
    # ------------------------------------------------------------------

    def _show_context_menu(self, point):
        index = self.tree.indexAt(point)
        menu = QMenu(self)

        if index.isValid():
            source_index = self.proxy_model.mapToSource(index)
            path = self.fs_model.filePath(source_index)
            is_dir = self.fs_model.isDir(source_index)
            target_dir = path if is_dir else os.path.dirname(path)

            new_file_action = QAction("New File", self)
            new_file_action.triggered.connect(
                lambda: self._create_new_file(target_dir)
            )
            menu.addAction(new_file_action)

            new_folder_action = QAction("New Folder", self)
            new_folder_action.triggered.connect(
                lambda: self._create_new_folder(target_dir)
            )
            menu.addAction(new_folder_action)

            menu.addSeparator()

            rename_action = QAction("Rename", self)
            rename_action.triggered.connect(lambda: self._rename_item(index))
            menu.addAction(rename_action)

            delete_action = QAction("Delete", self)
            delete_action.triggered.connect(lambda: self._delete_item(index))
            menu.addAction(delete_action)

        menu.exec(self.tree.mapToGlobal(point))

    # ------------------------------------------------------------------
    # File / folder operations
    # ------------------------------------------------------------------

    def _create_new_file(self, dir_path: str):
        name, ok = QInputDialog.getText(
            self, "New File", "Filename:", text="untitled.f90"
        )
        if ok and name:
            full_path = os.path.join(dir_path, name)
            try:
                with open(full_path, "w"):
                    pass
            except OSError as e:
                QMessageBox.critical(self, "Error", f"Could not create file:\n{e}")

    def _create_new_folder(self, dir_path: str):
        name, ok = QInputDialog.getText(self, "New Folder", "Folder name:")
        if ok and name:
            full_path = os.path.join(dir_path, name)
            try:
                os.mkdir(full_path)
            except OSError as e:
                QMessageBox.critical(self, "Error", f"Could not create folder:\n{e}")

    def _rename_item(self, index):
        """Trigger inline rename via QFileSystemModel."""
        self.fs_model.setReadOnly(False)
        self.tree.edit(index)
        self.fs_model.setReadOnly(True)

    def _delete_item(self, index):
        source_index = self.proxy_model.mapToSource(index)
        path = self.fs_model.filePath(source_index)
        reply = QMessageBox.question(
            self,
            "Delete",
            f"Are you sure you want to delete '{os.path.basename(path)}'?",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No,
        )
        if reply == QMessageBox.StandardButton.Yes:
            if not self.fs_model.remove(source_index):
                QMessageBox.critical(
                    self, "Error", f"Could not delete '{os.path.basename(path)}'."
                )

    # ------------------------------------------------------------------
    # Settings
    # ------------------------------------------------------------------

    def _load_settings(self):
        last_dir = self.settings.get("browser", "last_directory")
        if last_dir and os.path.isdir(last_dir):
            self.navigate_to(last_dir)
        else:
            self.navigate_to(QDir.homePath())

