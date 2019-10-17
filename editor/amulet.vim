if exists("b:did_ftplugin")
  finish
endif

setlocal shiftwidth=2
setlocal tabstop=2
setlocal expandtab
setlocal comments=s1:(*,mb:*,ex:*)
setlocal commentstring=(*\ %s\ *)

let b:did_ftplugin = 1

if has('nvim-0.3.2')
  let s:vtext_ns = nvim_create_namespace('amulet')
endif

let b:autostart_amc = has('nvim') ? 1 : 0
let b:amc_deps = []

hi! AmuletError cterm=underline ctermfg=red
hi! AmuletWarn cterm=underline ctermfg=yellow
hi! AmuletErrorVtxt ctermfg=red cterm=italic
hi! AmuletWarnVtxt ctermfg=yellow cterm=italic
sign define amuletError   text=! texthl=AmuletError
sign define amuletWarning text=* texthl=AmuletWarn

function! s:HighlightRange(group, line, start_col, length)
  " Vim columns are 1-indexed, but nvim's are 0-indexed
  if has('nvim-0.3.2')
    let start_col = str2nr(a:start_col) - 1
    let end_col = str2nr(a:length) + start_col
    let line = str2nr(a:line) - 1
    call nvim_buf_add_highlight(bufnr(''), s:vtext_ns, a:group, line, start_col, end_col)
  else
    call matchaddpos(a:group, [ [a:line, a:start_col, a:length] ])
  end
endfunction

function! AmuletStart(...)
  if has('nvim')
    if exists("b:amulet_pid")
      return 0
    end
    let b:amulet_pid = jobstart(["amc", "repl", "--port", a:0 == 1 ? a:1 : 6000])
    let b:amulet_port = a:0 == 1 ? a:1 : 6000
  else
    echo "Having vim manage the compiler is only supported on nvim"
  end
endfunction

function! AmuletStop()
  if has('nvim')
    if exists("b:amulet_pid")
      call jobstop(b:amulet_pid)
      unlet b:amulet_pid b:amulet_port
    end
  else
    echo "Having vim manage the compiler is only supported on nvim"
  end
endfunction

function! s:CallAmc(...)
  let cmd = shellescape(join(a:000))

  if exists("b:amulet_pid")
    let cmd = cmd . " --port " . b:amulet_port
  end

  let res = system('amc connect ' . cmd)
  if res =~ "Failed to connect to server on port"
    if has('nvim') && b:autostart_amc == 1
      call AmuletStart()
      return call("s:CallAmc", a:000)
    else
      throw "Couldn't connect to a running amc"
    end
  elseif res =~ "Invalid option `--client`"
    throw "Installed version of amc doesn't have client/server support"
  else
    return res
  endif
endfunction


" Trashes both qflist and matches
function! AmuletLoad(verbose, qf)
  let our_bufnr = bufnr('')
  let ourpos = getpos('.')
  silent write
  call clearmatches()
  call setqflist([])

  let file = expand("%:p")

  try
    let out = split(call("s:CallAmc", [":l"] + b:amc_deps + [file]), '\n')
  catch /version/
    echo "Your version of amc is too old to have client/server support"
  catch /amc/
    echo "Failed to connect to a running amc"
    return -1
  endtry

  let err_msg_pat = "\\v^" . file
  let nerrors = 0
  let nwarns = 0

  execute "sign unplace * file=" . expand("%p")
  if has('nvim-0.3.2')
    call nvim_buf_clear_namespace(0, s:vtext_ns, 0, -1)
  endif

  if len(out) > 1
    let idx = 0
    while idx < len(out)
      let line = out[idx]
      if line =~ err_msg_pat
        let err_msg_idx = idx + 1
        while match(out[err_msg_idx], "\\v([0-9]+)?\\s*\u2502") == 0
          let err_msg_idx += 1
        endwhile

        let next = match(out[err_msg_idx:], err_msg_pat)
        if next == -1
          let next = len(out)
        else
          let next += err_msg_idx
        endif
        let range = matchlist(line, "\\v\\[([0-9]+):([0-9]+) ..([0-9]+):([0-9]+)")
        let lineno = range[1]

        let err_msg = out[err_msg_idx]
        if err_msg_idx == idx + 1
          let idx += 1
        else
          let err_msg = join(map(out[err_msg_idx:next - 1], "trim(v:val)"))
          let idx = next
        endif

        caddexpr expand("%:p") . ":" . lineno . ":" . err_msg

        if range[1] == range[3]
          let group = "AmuletError"
          let sign = "amuletError"
          if match(line, "warning$") != -1
            let group = "AmuletWarn"
            let sign = "amuletWarning"
          end

          call s:HighlightRange(group, lineno, range[2], range[4] - range[2] + 1)
          execute "sign place " . idx . " line=" . lineno . " name=" . sign
          if has('nvim-0.3.2')
            call nvim_buf_set_virtual_text(0, s:vtext_ns, str2nr(lineno - 1), [[err_msg, group . "vtxt"]], {})
          endif
        endif

        if match(line, "warning$") != -1
          let nwarns += 1
        else
          let nerrors += 1
        end
      else
        let idx += 1
      endif
    endwhile

    let errs = (nerrors == 1) ? "error" : "errors"
    let warns = (nwarns == 1) ? "warning" : "warnings"

    if (nerrors == 0) && a:verbose
      echo "Beware: " . nwarns . " " . warns
    elseif nerrors != 0
      echo "Compilation failed with " . nerrors . " " . errs . " and " . nwarns . " " . warns
    end
    if (nerrors != 0 || nwarns != 0) && a:qf == 'qf'
      copen
    end
    call setpos('.', ourpos)
    return nerrors
  else
    if a:verbose
      echo "Loaded " . expand("%p") . " successfully."
    end
    call setpos('.', ourpos)
    return 0
  endif
endfunction

function! AmuletType()
  let ourpos = getpos('.')
  echo trim(s:CallAmc(":t", expand("<cword>")))
  call setpos('.', ourpos)
endfunction

function! AmuletEval(mode)
  let ourpos = getpos('.')
  if a:mode == 'v'
    let [line_start, column_start] = getpos("'<")[1:2]
    let [line_end, column_end] = getpos("'>")[1:2]

    if line_start != line_end
      return
    endif

    let expr = getline(line_start)[column_start - 1 : column_end - 1]
    echo trim(s:CallAmc(expr))
  else
    let expr = input("Evaluate: ")
    redraw
    echo trim(s:CallAmc(expr))
  end
  call setpos('.', ourpos)
endfunction

function! AmuletDep(...)
  for path in a:000
    if filereadable(path)
      call add(b:amc_deps, path)
    else
      echo "File " . file . " not readable by current user"
    endif
  endfor
  call AmuletLoad(1, '')
endfunction

function! AmuletParseDeps()
  let b:amc_deps = []
  let deplist = []
  silent keeppatterns %s/(\* amulet\.vim: \(.*\) \*)/\=add(deplist,submatch(1))/gne
  call call('AmuletDep', deplist)
endfunction

nnoremap <buffer> <silent> <LocalLeader>t :call AmuletType()<ENTER>
nnoremap <buffer> <silent> <LocalLeader>l :call AmuletLoad(1,'')<ENTER>
nnoremap <buffer> <silent> <LocalLeader>L :call AmuletLoad(0,'qf')<ENTER>
vnoremap <buffer> <silent> <LocalLeader>e :call AmuletEval('v')<ENTER>
nnoremap <buffer> <silent> <LocalLeader>e :call AmuletEval('n')<ENTER>
command! -nargs=? AmuletStartServer :call AmuletStart(<args>)
command! -complete=file -nargs=+ AmuletDepend :call AmuletDep(<f-args>)
command! -nargs=0 AmuletFindDepends :call AmuletParseDeps()
cnoreabbr amcdep AmuletDepend
