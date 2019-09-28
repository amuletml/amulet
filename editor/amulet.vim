if exists("b:did_ftplugin")
  finish
endif

setlocal shiftwidth=2
setlocal tabstop=2
setlocal expandtab
setlocal comments=s1:(*,mb:*,ex:*)
setlocal commentstring=(*\ %s\ *)

let b:did_ftplugin = 1

function! s:CallAmc(...)
  let cmd = shellescape(join(a:000))
  return system('amc --client ' . cmd)
endfunction

hi! AmuletError cterm=underline ctermfg=red
hi! AmuletWarn cterm=underline ctermfg=yellow
sign define amuletError   text=! texthl=AmuletError
sign define amuletWarning text=* texthl=AmuletWarn

" Trashes both qflist and matches
function! AmuletLoad(verbose, qf)
  let our_bufnr = bufnr()
  silent write
  call clearmatches()
  call setqflist([])

  let file = expand("%:p")
  let out = split(s:CallAmc(":l", file), '\n')
  let err_msg_pat = "\\v^" . file
  let nerrors = 0
  let nwarns = 0

  execute "sign unplace * file=" . expand("%p")

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

        if range[1] == range[3]
          if match(line, "warning$")
            call matchaddpos("AmuletWarn", [ [lineno, range[2], range[4] - range[2] + 1] ])
            execute "sign place " . idx . " line=" . lineno . " name=amuletWarning"
          else
            call matchaddpos("AmuletError", [ [lineno, range[2], range[4] - range[2] + 1] ])
            execute "sign place " . idx . " line=" . lineno . " name=amuletError"
          end
        endif

        if err_msg_idx == idx + 1
          caddexpr expand("%:p") . ":" . lineno . ":" . out[err_msg_idx]
          let idx += 1
        else
          let err_msg_list = map(out[err_msg_idx:next - 1], "trim(v:val)")
          caddexpr expand("%:p") . ":" . lineno . ":" . join(err_msg_list)
          let idx = next
        endif

        if match(line, "warning$")
          let nwarns += 1
        else
          let nerrors += 1
        end
      else
        let idx += 1
      endif
    endwhile

    if nerrors == 1
      let errs = "error"
    else
      let errs = "errors"
    end

    if nwarns == 1
      let warns = "warning"
    else
      let warns = "warnings"
    end

    if (nerrors == 0) && a:verbose
      echo "Beware: " . nwarns . " " . warns
    elseif nerrors != 0
      echo "Compilation failed with " . nerrors . " " . errs . " " . " and " . nwarns . " " . warns
    end
    if (nerrors != 0 || nwarns != 0) && a:qf == 'qf'
      copen
    end
    return nerrors
  else
    if a:verbose
      echo "Loaded " . expand("%p") . " successfully."
    end
    return 1
  endif
endfunction

function! AmuletType()
  let ok = AmuletLoad(0, '')
  if ok == 0
    echo trim(s:CallAmc(":t", expand("<cword>")))
  else
  end
endfunction

function! AmuletEval(mode)
  let ok = AmuletLoad(0, '')
  if ok != 0
    return
  end

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
endfunction

nnoremap <buffer> <silent> <LocalLeader>t :call AmuletType()<ENTER>
nnoremap <buffer> <silent> <LocalLeader>l :call AmuletLoad(1,'')<ENTER>
nnoremap <buffer> <silent> <LocalLeader>L :call AmuletLoad(0,'qf')<ENTER>
vnoremap <buffer> <silent> <LocalLeader>e :call AmuletEval('v')<ENTER>
nnoremap <buffer> <silent> <LocalLeader>e :call AmuletEval('n')<ENTER>
