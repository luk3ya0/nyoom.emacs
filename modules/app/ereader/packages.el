;;; app/ereader/packages.el -*- lexical-binding: t; -*-

(package! nov)
(package! pangu-spacing)
(package! calibredb
  :recipe (:host github :repo "chenyanming/calibredb.el")
  :pin "a3b04c0c37b1e8ceff2472e21a3579e64e944528")
(package! visual-fill-column :pin "6fa9e7912af412533aec0da8b8f62c227f9f3f54")
(package! mixed-pitch :pin "519e05f74825abf04b7d2e0e38ec040d013a125a")
