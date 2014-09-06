" Haskell filetype plugin for Parsing.hs, LogicalForm.hs
" Last Change:	2000 Oct 15
" Maintainer:	Bram Moolenaar <Bram@vim.org>
" License:	This file is placed in the public domain.

let s:save_cpo = &cpo
set cpo&vim

set cpoptions+=M		"ignore backslashing in parentheses matching

if exists("loaded_haskell_after")
finish
endif
let loaded_haskell_after = 1

fu! CommonNoun() 
	let noun = expand('<cword>')
	let up = Ucfirst(noun)
	call setline(".", "lfCN " . up . '	= \t -> Atom "' . noun . '" [t]')
endfunction
" ino <LocalLeader>N <Esc>:call CommonNoun()<CR>o

fu! Interp() 
	let relation = expand('<cword>')
	call inputsave()
	let key = input("Predicate Places: '1', '2', '3', '4' or  (C)ase")
	call inputrestore()
	let feature = get( {'1': '1', '2': '2', '3': '3', '4': '4'}, key )
	if feature == '1'
		call setline(".", "\t, ( \"" . relation . "\",\t\\[x]\t-> predid1 \"" .
			\ relation . "\" x\t)")
	elseif feature == '2'
		call setline(".", "\t, ( \"" . relation . "\",\t\\[x,y]\t-> predid2 \"" .
			\ relation . "\" y x\t)")
	elseif feature == '3'
		call setline(".", "\t, ( \"" . relation . "\",\t\\[x,y,z]\t-> predid3 \"" .
			\ relation . "\" z y x\t)")
	elseif feature == '3'
		call setline(".", "\t, ( \"" . relation . "\",\t\\[x,y,z,w]\t-> predid4 \"" .
			\ relation . "\" w z y x\t)")
	else
		call setline(".", "\t, ( \"" . relation . "\",\t\\args -> case args of")
		call append(".", [ "\t\t[x,y,z,w]\t-> predid4 \"" . relation ."\" w z y x", "\t\t[x,y,z]\t-> (forgetful4 . predid4) \"" . relation . "\" z y x", "\t\t[x,y]\t-> (forgetful3 . forgetful4 . predid4) \"" . relation . "\" y x", "\t\t[x]\t-> (forgetful2 . forgetful3 . forgetful4 . predid4) \"" . relation . "\" x\t)" ])
	endif
endfunction
ino <LocalLeader>I <Esc>:call Interp()<CR>o

fu! Predid() 
	let relation = expand('<cword>')
	call inputsave()
	let key = input("Predicate Places: '1', '2', '3', '4' or  (C)ase")
	call inputrestore()
	let feature = get( {'1': '1', '2': '2', '3': '3', '4': '4'}, key )
	call setline(".", "\t, ( \"" . relation . "\",\tpred" . feature . " [] )")
endfunction
" ino <LocalLeader>p <Esc>:call Predid()<CR>o

fu! Inflect()
	let words = split(getline('.'), '\s\+')
	let inflection = "\t, (\"" . get(words, 0) . "\",\t\"" . get(words, 1) . "\" )"
	call setline('.', inflection)
endfunction
ino <LocalLeader>N <Esc>:call Inflect()<CR>o


fu! Constantterm()
	let string = expand('<cword>')
	let cap = Ucfirst( string )
	let term = 'G' . string
	let lnum = line('.')
	let prevline = getline(prevnonblank(lnum - 1))
	let mx = '\(\w\+\).*'
	let line = matchstr(prevline, mx)
	let catlist = substitute(line, mx, '\1', '')
	if catlist == 'entity_list'
		call setline(".", catlist . " " . term .
			\ "\t= ent_ided \"" . cap . "\"")
	else
		call setline(".", catlist . " " . term .
			\ "\t= \"" . string . "\"")
	endif
endfunction
ino <LocalLeader>C <Esc>:call Constantterm()<CR>o

fu! Name()
	let lexeme = expand('<cword>')
	call inputsave()
	let key = input("Feature: '(M)asc', '(F)em', or '(N)eutr' ")
	call inputrestore()
	let feature = get( {'m': 'Masc', 'f': 'Fem', 'n': 'Neutr'}, key )
	call setline(".", "\t, [Cat \"" . lexeme . "\"	\"NP\" [Thrd," . feature . ",Sg] []]")
endf
ino <LocalLeader>P <Esc>:call Name()<CR>o

fu! Object()
	let lexeme = expand('<cword>')
	call inputsave()
	let key = input("Feature: 'CN', or 'NP' ")
	call inputrestore()
	let uncount = get( {'c': 'CN', 'n': 'NP'}, key )
	let key = input("Feature: 'Sg', or 'Pl' ")
	call inputrestore()
	let number = get( {'s': 'Sg', 'p': 'Pl'}, key )
	call setline(".", "\t, [Cat \"" . lexeme . "\"	\"" . uncount . "\" [Thrd,Neutr," . number . "] []]")
endf
ino <LocalLeader>O <Esc>:call Object()<CR>o

fu! Transitive()
	let lexeme = expand('<cword>')
	call inputsave()
	let key = input("Feature: '(T)ense' or '(I)nfl' ")
	call inputrestore()
	let feature = get( {'t': 'Tense', 'i': 'Infl'}, key )
	call setline(".", "\t[Cat \"" . lexeme . "\"	\"VP\" [" . feature . "] [Cat \"_\" \"NP\" [AccOrDat] []]],")
endf
ino <LocalLeader>T <Esc>:call Transitive()<CR>o

fu! Predicate()
	let name = expand('<cword>')
	call inputsave()
	let key = input("Predicate Places: '1', '2', or '3' ")
	call inputrestore()
	let feature = get( {'1': '1', '2': '2', '3': '3'}, key )
	let initial = Firstletter( Ucfirst( name ) )
	call setline(".", name . "\t= pred" . feature . " []")
endf
ino <LocalLeader>Pr <Esc>:call Predicate()<CR>$i

fu! Oneword()
	let lnum = line('.')
	let words = split(getline(lnum), '\s\+')
	let wordlist = join( words, '":"')
	let oneword = join( words, '_')
	call setline(".", "preproc (\"" . wordlist . "\":xs)\t= \"" . oneword . "\" : preproc xs")
endf
ino <LocalLeader>1 <Esc>:call Oneword()<CR>o

fu! Populate_pn(word, module)
	call append(line('.'), "\t" . down_name . "\t= mk" . category . "( mk" . super_cat . " masculine (mk" . super_cat ." \"" . a:word . "\") );")
endf

fu! Populate_ap_like(word, module, category, super_cat)
	call append(line('.'), "\t" . down_name . "\t= mk" . a:category . "( mk" . a:super_cat . " \"" . a:word . "\");")
endf

fu! Populate(module)
	let quoted_word = matchstr( getline('.'), "\".*\"")
	if quoted_word == ""
		let quoted_word = expand('<cWORD>')
	endif
	let word = substitute( quoted_word, "\"", "", "g")
	call inputsave()
	let key = input("Cat: '(A)', '(U)N', '(C)N', '(P)N', '(V)*', a(D)v, p(R)ep ")
	call inputrestore()
	let category = get( {'a': 'A', 'u': 'N', 'c': 'CN', 'p': 'PN', 'v': 'V', 'd': "Adv", 'r': "Prep"}, key )
	call setline('.', word . "\t: " . category . ";")
	let word_buf = bufnr("%")
	let save_cursor = getpos(".")
	let mark = strpart(category, 0, 1)
	let word_lnum = line("'" . mark)
	call append(word_lnum,'')
	call setline((word_lnum+1), "\t, \"" . word . "\"")
	call setpos("'" . mark, [0, (word_lnum+1), 1, 0])

	let lc_name = word
	let down_name = substitute(lc_name, '\(-\| \)', "_", "g")

	let ab_gf = bufnr( "/" . a:module . "\.gf")
	execute "buffer" ab_gf
	let last_line = line("$")
	call cursor(last_line, 1)
	call search(category, "bc")
	call append(line('.'), "\t" . down_name . "\t: " . category . ";")

	let ab_eng_gf = bufnr( a:module . "Eng.gf")
	execute "buffer" ab_eng_gf
	let last_line = line("$")
	call cursor(last_line, 1)
	call search(category, "bc")
	if category == "PN"
		call Populate_pn(word, a:module, "PN", "N") 
	elseif category == "A"
		call Populate_ap_like(word, a:module, "AP", "A") 
	elseif category == "CN"
		call Populate_ap_like(word, a:module, "CN", "N") 
	else 
		call append(line('.'), "\t" . down_name . "\t= mk" . category . " \"" . word . "\";")
	endif

	execute "buffer" word_buf
	call setpos('.', save_cursor)

endf
nn <LocalLeader>p <Esc>:call Populate("Candidate")<CR>j

" put words in DicksonI.gf, LexDickson.gf, LexDicksonEng.gf

nn <LocalLeader>M yiwPV>A	: PN;:w:2bn"0PV>A      = mkPN "0pA_N;o:w:bn"0PV>A_N  : N;lO: "<80>kbw:bn"0PA_N       = mkN "0";bb~lV>o:w:bn"0PV>I, "l~lA":w:        brew}w

" put words in categories in WordsCharacters.hs and in Dickson.gf

nn <LocalLeader>e yiw'aA  , "^R0"^M^Cma''DI                                  -- ^R0       : CN;^Cyy:brewind^M'bPo^Cmb^M:5bn^M^M


let &cpo = s:save_cpo
