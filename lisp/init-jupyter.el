;; init-jupyter.el --- Emacs Jupyter Configurations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;; Emacs Jupyter Configurations
;;

;;; Code:

;; NOTE: Jupyter and external kernels
;; $ conda create -n jupyter python={VERSION}
;; $ conda jupyterlab nb_conda_kernels

;; NOTE: jupyter in virtual environment
;; (conda-env-activate "YOUR_ENV") for example

;; NOTE: nb_conda_kernel
;;  set `kernelspec_path` for tools other than jupyter notebooks
;; https://github.com/Anaconda-Platform/nb_conda_kernels#use-with-nbconvert-voila-papermill
;; 1) Create a configuration file for jupyter named `jupyter_config.json` in the folder returned by `jupyter --config-dir`.
;; 2) Add the following configuration to install all kernel spec for the current user:
;; {
;;   "CondaKernelSpecManager": {
;;     "kernelspec_path": "--user"
;;   }
;; }
;; 3) Execute the command (or open the classical Notebook or JupyterLab UI):
;; `python -m nb_conda_kernels list`
;; 4) Check that the conda environment kernels are discovered by `jupyter`:
;; `jupyter kernelspec list`

;; NOTE: build project local IJulia
;; julia> using Pkg
;; julia> Pkg.activate("YOUR_JULIA_PROJECT_ENV")
;; julia> ENV["JUPYTER"]="YOUR_JUPYTER_CMD_PATH"
;; julia> using IJulia
;; julia> Pkg.build("IJulia")

;; NOTE: run project local IJulia in jupyter
;; https://github.com/JuliaLang/julia/issues/29513
;; (setenv "JULIA_LOAD_PATH" "YOUR_PROJECT_PATH")

;; An interface ot communicate with Jupyter kernels in Emacs
(use-package jupyter
  :commands (jupyter-run-repl jupyter-connect-repl))

;; TODO: config ein
(use-package ein
  :defer t)


(provide 'init-jupyter)

;;; init-jupyter.el ends here
