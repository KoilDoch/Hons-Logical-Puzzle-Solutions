\NeedsTeXFormat{LaTeX2e}
% when committing a new version, remember to change the date below. The date is in YYYY/MM/DD format.
\ProvidesClass{macsthesis}[2022/10/05 Class for writing BSc and MSc theses at HWU MACS department]

% the class is based on KOMA-script's report class
% A4 with 11pt font, as required by the guidelines
\LoadClass[a4paper,11pt]{scrreprt}

% commands for adding degree and supervisor information
\makeatletter
\newcommand\degree[1]{\renewcommand\@degree{#1}}
\newcommand\@degree{}
\newcommand\supervisor[1]{\renewcommand\@supervisor{#1}}
\newcommand\@supervisor{}
\makeatother


% ifthen used for processing options
\RequirePackage{ifthen}

% headers on or off (default: off)
\makeatletter
\newboolean{@header}
\setboolean{@header}{false} 
\DeclareOption{header}{\setboolean{@header}{true}}
\makeatother

% NO MORE OPTION DECLARATIONS BEYOND THIS POINT!
\ProcessOptions\relax

% header setup
\usepackage[headsepline]{scrlayer-scrpage}
\makeatletter
\ihead{\@title}
\chead{}
\ohead{\@author}
\ifoot{}
\cfoot{\pagemark}
\ofoot{}
\makeatother

% getting \frontmatter and \mainmatter commands working
\newcommand{\frontmatter}{\pagenumbering{roman}\pagestyle{plain}}
\makeatletter
\newcommand{\mainmatter}{\newpage\pagenumbering{arabic}\ifthenelse{\boolean{@header}}{\pagestyle{scrheadings}}{}}
\makeatother

% allow unicode input
\RequirePackage[utf8]{inputenc}

% T1 fonts
\RequirePackage[T1]{fontenc}

% UK English spelling rules
\RequirePackage[UKenglish]{babel}

% set the margin to 1 inch, as required by the guidelines
\RequirePackage[margin=1in]{geometry}

% bibliography style and naming ("References" as per the guidelines)
% TODO: allow users to choose the bibliography style
\RequirePackage[round]{natbib}
\renewcommand{\bibsection}{\chapter*{References}}

% hyperref set up to avoid boxes around hyperlinks; colors links blue instead.
\RequirePackage{xcolor}
\definecolor{darkblue}{HTML}{002E67}
\RequirePackage[breaklinks,unicode=true,colorlinks=true,linkcolor=darkblue,citecolor=darkblue,urlcolor=blue]{hyperref}

% for double spacing
\RequirePackage{setspace}
\AtBeginDocument{\doublespacing}

% chapter and (sub)*section commands which suppress double spacing in chapter and (sub)*section headings
% Unnumbered chapters appear in the TOC (useful for the abstract), other unnumbered entries do not
% TODO: figure out how to override the \chapter, \section, etc,
\RequirePackage{suffix}
\newcommand{\Chapter}[1]{\singlespacing\chapter{#1}\doublespacing}
\WithSuffix\newcommand\Chapter*[1]{\singlespacing\chapter*{#1}\addcontentsline{toc}{chapter}{#1}\doublespacing}
\newcommand{\Section}[1]{\singlespacing\section{#1}\doublespacing}
\WithSuffix\newcommand\Section*[1]{\singlespacing\section*{#1}\doublespacing}
\newcommand{\Subsection}[1]{\singlespacing\subsection{#1}\doublespacing}
\WithSuffix\newcommand\Subsection*[1]{\singlespacing\subsection*{#1}\doublespacing}
\newcommand{\Subsubsection}[1]{\singlespacing\subsubsection{#1}\doublespacing}
\WithSuffix\newcommand\Subsubsection*[1]{\singlespacing\subsubsection*{#1}\doublespacing}


% for testing purposes
\RequirePackage{lipsum}

