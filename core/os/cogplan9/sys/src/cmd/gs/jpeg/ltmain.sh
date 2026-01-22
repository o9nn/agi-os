progname=`$echo "$0" | sed 's%^.*/%%'`
modename="$progname"
PROGRAM=ltmain.sh
PACKAGE=libtool
VERSION=1.2
default_mode=
help="Try \`$progname --help' for more information."
magic="%%%MAGIC variable%%%"
mkdir="mkdir"
mv="mv -f"
rm="rm -f"
Xsed='sed -e s/^X//'
sed_quote_subst='s/\([\\`\\"$\\\\]\)/\\\1/g'
if test "${LC_ALL+set}" = set; then LC_ALL=C; export LC_ALL; fi
if test "${LANG+set}"   = set; then LANG=C;   export LANG;   fi
if test "$LTCONFIG_VERSION" != "$VERSION"; then
  echo "$modename: ltconfig version \`$LTCONFIG_VERSION' does not match $PROGRAM version \`$VERSION'" 1>&2
  echo "Fatal configuration error.  See the $PACKAGE docs for more information." 1>&2
  exit 1
fi
if test "$build_libtool_libs" != yes && test "$build_old_libs" != yes; then
  echo "$modename: not configured to build any kind of library" 1>&2
  echo "Fatal configuration error.  See the $PACKAGE docs for more information." 1>&2
  exit 1
fi
mode=$default_mode
nonopt=
prev=
prevopt=
run=
show="$echo"
show_help=
execute_dlfiles=
while test $
do
  arg="$1"
  shift
  case "$arg" in
  -*=*) optarg=`$echo "X$arg" | $Xsed -e 's/[-_a-zA-Z0-9]*=//'` ;;
  *) optarg= ;;
  esac
  if test -n "$prev"; then
    case "$prev" in
    execute_dlfiles)
      eval "$prev=\"\$$prev \$arg\""
      ;;
    *)
      eval "$prev=\$arg"
      ;;
    esac
    prev=
    prevopt=
    continue
  fi
  case "$arg" in
  --help)
    show_help=yes
    ;;
  --version)
    echo "$PROGRAM (GNU $PACKAGE) $VERSION"
    exit 0
    ;;
  --dry-run | -n)
    run=:
    ;;
  --features)
    echo "host: $host"
    if test "$build_libtool_libs" = yes; then
      echo "enable shared libraries"
    else
      echo "disable shared libraries"
    fi
    if test "$build_old_libs" = yes; then
      echo "enable static libraries"
    else
      echo "disable static libraries"
    fi
    exit 0
    ;;
  --finish) mode="finish" ;;
  --mode) prevopt="--mode" prev=mode ;;
  --mode=*) mode="$optarg" ;;
  --quiet | --silent)
    show=:
    ;;
  -dlopen)
    prevopt="-dlopen"
    prev=execute_dlfiles
    ;;
  -*)
    $echo "$modename: unrecognized option \`$arg'" 1>&2
    $echo "$help" 1>&2
    exit 1
    ;;
  *)
    nonopt="$arg"
    break
    ;;
  esac
done
if test -n "$prevopt"; then
  $echo "$modename: option \`$prevopt' requires an argument" 1>&2
  $echo "$help" 1>&2
  exit 1
fi
if test -z "$show_help"; then
  if test -z "$mode"; then
    case "$nonopt" in
    *cc | *++ | gcc* | *-gcc*)
      mode=link
      for arg
      do
        case "$arg" in
        -c)
           mode=compile
           break
           ;;
        esac
      done
      ;;
    *db | *dbx)
      mode=execute
      ;;
    *install*|cp|mv)
      mode=install
      ;;
    *rm)
      mode=uninstall
      ;;
    *)
      test -n "$execute_dlfiles" && mode=execute
      if test -z "$mode"; then
        if test -n "$nonopt"; then
          $echo "$modename: warning: cannot infer operation mode from \`$nonopt'" 1>&2
        else
          $echo "$modename: warning: cannot infer operation mode without MODE-ARGS" 1>&2
        fi
      fi
      ;;
    esac
  fi
  if test -n "$execute_dlfiles" && test "$mode" != execute; then
    $echo "$modename: unrecognized option \`-dlopen'" 1>&2
    $echo "$help" 1>&2
    exit 1
  fi
  generic_help="$help"
  help="Try \`$modename --help --mode=$mode' for more information."
  case "$mode" in
  compile)
    modename="$modename: compile"
    base_compile=
    lastarg=
    srcfile="$nonopt"
    suppress_output=
    for arg
    do
      case "$arg" in
      -o)
	$echo "$modename: you cannot specify the output filename with \`-o'" 1>&2
	$echo "$help" 1>&2
	exit 1
	;;
      -static)
	build_libtool_libs=no
	build_old_libs=yes
	continue
	;;
      esac
      lastarg="$srcfile"
      srcfile="$arg"
      lastarg=`$echo "X$lastarg" | $Xsed -e "$sed_quote_subst"`
      case "$lastarg" in
      *[\[\~\
	lastarg="\"$lastarg\""
	;;
      esac
      if test -z "$base_compile"; then
	base_compile="$lastarg"
      else
	base_compile="$base_compile $lastarg"
      fi
    done
    libobj=`$echo "X$srcfile" | $Xsed -e 's%^.*/%%'`
    xform='[cCFSfms]'
    case "$libobj" in
    *.ada) xform=ada ;;
    *.adb) xform=adb ;;
    *.ads) xform=ads ;;
    *.asm) xform=asm ;;
    *.c++) xform=c++ ;;
    *.cc) xform=cc ;;
    *.cpp) xform=cpp ;;
    *.cxx) xform=cxx ;;
    *.f90) xform=f90 ;;
    *.for) xform=for ;;
    esac
    libobj=`$echo "X$libobj" | $Xsed -e "s/\.$xform$/.lo/"`
    case "$libobj" in
    *.lo) obj=`$echo "X$libobj" | $Xsed -e 's/\.lo$/.o/'` ;;
    *)
      $echo "$modename: cannot determine name of library object from \`$srcfile'" 1>&2
      exit 1
      ;;
    esac
    if test -z "$base_compile"; then
      $echo "$modename: you must specify a compilation command" 1>&2
      $echo "$help" 1>&2
      exit 1
    fi
    if test "$build_old_libs" = yes; then
      $run $rm $obj $libobj
      trap "$run $rm $obj $libobj; exit 1" 1 2 15
    else
      $run $rm $libobj
      trap "$run $rm $libobj; exit 1" 1 2 15
    fi
    if test "$build_libtool_libs" = yes; then
      fbsd_hideous_sh_bug=$base_compile
      $show "$base_compile$pic_flag -DPIC $srcfile"
      if $run eval "$base_compile\$pic_flag -DPIC \$srcfile"; then :
      else
        test -n "$obj" && $run $rm $obj
        exit 1
      fi
      if test -z "$pic_flag"; then
        $show "$LN_S $obj $libobj"
        $run $LN_S $obj $libobj
        exit $?
      fi
      $show "$mv $obj $libobj"
      $run $mv $obj $libobj || exit 1
      suppress_output=' >/dev/null 2>&1'
    fi
    if test "$build_old_libs" = yes; then
      $show "$base_compile $srcfile$suppress_output"
      if $run eval "$base_compile \$srcfile$suppress_output"; then :
      else
        $run $rm $obj $libobj
        exit 1
      fi
    fi
    if test "$build_libtool_libs" != yes; then
      $show "echo timestamp > $libobj"
      $run eval "echo timestamp > \$libobj" || exit $?
    fi
    exit 0
    ;;
  link)
    modename="$modename: link"
    CC="$nonopt"
    allow_undefined=yes
    compile_command="$CC"
    finalize_command="$CC"
    compile_shlibpath=
    finalize_shlibpath=
    deplibs=
    dlfiles=
    dlprefiles=
    export_dynamic=no
    hardcode_libdirs=
    libobjs=
    link_against_libtool_libs=
    ltlibs=
    objs=
    prev=
    prevarg=
    release=
    rpath=
    perm_rpath=
    temp_rpath=
    vinfo=
    for arg
    do
      case "$arg" in
      -all-static | -static)
        if test "X$arg" = "X-all-static" && test "$build_libtool_libs" = yes && test -z "$link_static_flag"; then
	    $echo "$modename: warning: complete static linking is impossible in this configuration" 1>&2
        fi
        build_libtool_libs=no
	build_old_libs=yes
        break
        ;;
      esac
    done
    test -n "$old_archive_from_new_cmds" && build_old_libs=yes
    for arg
    do
      if test -n "$prev"; then
        case "$prev" in
        output)
          compile_command="$compile_command @OUTPUT@"
          finalize_command="$finalize_command @OUTPUT@"
          ;;
        esac
        case "$prev" in
        dlfiles|dlprefiles)
          case "$arg" in
          *.la | *.lo) ;;
          *)
            dlprefiles="$dlprefiles $arg"
            test "$prev" = dlfiles && dlfiles="$dlfiles $arg"
            prev=
            ;;
          esac
          ;;
	release)
	  release="-$arg"
	  prev=
	  continue
	  ;;
        rpath)
          rpath="$rpath $arg"
	  prev=
	  continue
	  ;;
        *)
          eval "$prev=\"\$arg\""
          prev=
          continue
          ;;
        esac
      fi
      prevarg="$arg"
      case "$arg" in
      -all-static)
	if test -n "$link_static_flag"; then
          compile_command="$compile_command $link_static_flag"
	  finalize_command="$finalize_command $link_static_flag"
        fi
        continue
	;;
      -allow-undefined)
	$echo "$modename: \`-allow-undefined' is deprecated because it is the default" 1>&2
	continue
	;;
      -dlopen)
        prev=dlfiles
        continue
        ;;
      -dlpreopen)
        prev=dlprefiles
        continue
        ;;
      -export-dynamic)
        if test "$export_dynamic" != yes; then
          export_dynamic=yes
	  if test -n "$export_dynamic_flag_spec"; then
	    eval arg=\"$export_dynamic_flag_spec\"
	  else
	    arg=
	  fi
	  compile_command="$compile_command @SYMFILE@"
	  finalize_command="$finalize_command @SYMFILE@"
        fi
        ;;
      -L*)
        dir=`$echo "X$arg" | $Xsed -e 's%^-L\(.*\)$%\1%'`
        case "$dir" in
        /* | [A-Za-z]:\\*)
          ;;
        *)
          $echo "$modename: \`-L$dir' cannot specify a relative directory" 1>&2
          exit 1
          ;;
        esac
        deplibs="$deplibs $arg"
        ;;
      -l*) deplibs="$deplibs $arg" ;;
      -no-undefined)
	allow_undefined=no
	continue
	;;
      -o) prev=output ;;
      -release)
	prev=release
	continue
	;;
      -rpath)
        prev=rpath
        continue
        ;;
      -static)
	if test -z "$pic_flag" && test -n "$link_static_flag"; then
          compile_command="$compile_command $link_static_flag"
	  finalize_command="$finalize_command $link_static_flag"
        fi
	continue
	;;
      -version-info)
        prev=vinfo
        continue
        ;;
      -* | +*)
	arg=`$echo "X$arg" | $Xsed -e "$sed_quote_subst"`
	case "$arg" in
	*[\[\~\
	  arg="\"$arg\""
	  ;;
	esac
        ;;
      *.o | *.a)
        objs="$objs $arg"
        ;;
      *.lo)
	if test "$prev" = dlfiles; then
	  dlfiles="$dlfiles $arg"
	  if test "$build_libtool_libs" = yes; then
	    prev=
	    continue
	  else
	    prev=dlprefiles
	  fi
	fi
	if test "$prev" = dlprefiles; then
	  dlprefiles="$dlprefiles "`$echo "X$arg" | $Xsed -e 's/\.lo$/\.o/'`
	  prev=
	fi
	libobjs="$libobjs $arg"
        ;;
      *.la)
        dlname=
        libdir=
        library_names=
        old_library=
        if (sed -e '2q' $arg | egrep '^
        else
          $echo "$modename: \`$arg' is not a valid libtool archive" 1>&2
          exit 1
        fi
        case "$arg" in
        */* | *\\*) . $arg ;;
        *) . ./$arg ;;
        esac
        if test -z "$libdir"; then
          $echo "$modename: \`$arg' contains no -rpath information" 1>&2
          exit 1
        fi
        linklib=
        for l in $old_library $library_names; do
          linklib="$l"
        done
        if test -z "$linklib"; then
          $echo "$modename: cannot find name of link library for \`$arg'" 1>&2
          exit 1
        fi
        name=`$echo "X$arg" | $Xsed -e 's%^.*/%%' -e 's/\.la$//' -e 's/^lib//'`
        dir=`$echo "X$arg" | $Xsed -e 's%/[^/]*$%%'`
        if test "X$dir" = "X$arg"; then
          dir="$objdir"
        else
          dir="$dir/$objdir"
        fi
        if test "$prev" = dlfiles; then
          dlfiles="$dlfiles $arg"
          if test -z "$dlname"; then
            prev=dlprefiles
          else
	    compile_command="$compile_command$dependency_libs"
	    finalize_command="$finalize_command$dependency_libs"
            prev=
            continue
          fi
        fi
        if test "$prev" = dlprefiles; then
          if test -n "$old_library"; then
            dlprefiles="$dlprefiles $dir/$old_library"
          else
            dlprefiles="$dlprefiles $dir/$linklib"
          fi
          prev=
        fi
        if test "$build_libtool_libs" = yes && test -n "$library_names"; then
          link_against_libtool_libs="$link_against_libtool_libs $arg"
          if test -n "$shlibpath_var"; then
            case "$temp_rpath " in
            *" $dir "*) ;;
            *) temp_rpath="$temp_rpath $dir" ;;
            esac
          fi
          if test -n "$hardcode_libdir_flag_spec"; then
            if test -n "$hardcode_libdir_separator"; then
              if test -z "$hardcode_libdirs"; then
                hardcode_libdirs="$libdir"
                libdir="@HARDCODE_LIBDIRS@"
              else
		case "$hardcode_libdir_separator$hardcode_libdirs$hardcode_libdir_separator" in
		*"$hardcode_libdir_separator$libdir$hardcode_libdir_separator"*)
		  ;;
		*)
		  hardcode_libdirs="$hardcode_libdirs$hardcode_libdir_separator$libdir"
		  ;;
		esac
                libdir=
              fi
            fi
            if test -n "$libdir"; then
              eval flag=\"$hardcode_libdir_flag_spec\"
              compile_command="$compile_command $flag"
              finalize_command="$finalize_command $flag"
            fi
          elif test -n "$runpath_var"; then
            case "$perm_rpath " in
            *" $libdir "*) ;;
            *) perm_rpath="$perm_rpath $libdir" ;;
            esac
          fi
          case "$hardcode_action" in
          immediate)
            if test "$hardcode_direct" = no; then
              compile_command="$compile_command $dir/$linklib"
            elif test "$hardcode_minus_L" = no; then
              compile_command="$compile_command -L$dir -l$name"
            elif test "$hardcode_shlibpath_var" = no; then
              compile_shlibpath="$compile_shlibpath$dir:"
              compile_command="$compile_command -l$name"
            fi
            ;;
          relink)
            case "$dir" in
            /* | [A-Za-z]:\\*) ;;
            *)
              absdir=`cd "$dir" && pwd`
              if test -z "$absdir"; then
                $echo "$modename: cannot determine absolute directory name of \`$dir'" 1>&2
                exit 1
              fi
              dir="$absdir"
              ;;
            esac
            if test "$hardcode_direct" = yes; then
              compile_command="$compile_command $dir/$linklib"
            elif test "$hardcode_minus_L" = yes; then
              compile_command="$compile_command -L$dir -l$name"
            elif test "$hardcode_shlibpath_var" = yes; then
              compile_shlibpath="$compile_shlibpath$dir:"
              compile_command="$compile_command -l$name"
            fi
            ;;
          *)
            $echo "$modename: \`$hardcode_action' is an unknown hardcode action" 1>&2
            exit 1
            ;;
          esac
          if test "$hardcode_direct" = yes; then
            finalize_command="$finalize_command $libdir/$linklib"
          elif test "$hardcode_minus_L" = yes; then
            finalize_command="$finalize_command -L$libdir -l$name"
          elif test "$hardcode_shlibpath_var" = yes; then
            finalize_shlibpath="$finalize_shlibpath$libdir:"
            finalize_command="$finalize_command -l$name"
          else
            finalize_command="$finalize_command -L$libdir -l$name"
          fi
        else
          if test -n "$pic_flag" && test -z "$old_library"; then
            $echo "$modename: cannot find static library for \`$arg'" 1>&2
            exit 1
          fi
	  if test "$hardcode_direct" != unsupported; then
	    test -n "$old_library" && linklib="$old_library"
	    compile_command="$compile_command $dir/$linklib"
	    finalize_command="$finalize_command $dir/$linklib"
	  else
	    compile_command="$compile_command -L$dir -l$name"
	    finalize_command="$finalize_command -L$dir -l$name"
	  fi
        fi
	compile_command="$compile_command$dependency_libs"
	finalize_command="$finalize_command$dependency_libs"
	continue
        ;;
      *)
	arg=`$echo "X$arg" | $Xsed -e "$sed_quote_subst"`
	case "$arg" in
	*[\[\~\
	  arg="\"$arg\""
	  ;;
	esac
        ;;
      esac
      if test -n "$arg"; then
	compile_command="$compile_command $arg"
	finalize_command="$finalize_command $arg"
      fi
    done
    if test -n "$prev"; then
      $echo "$modename: the \`$prevarg' option requires an argument" 1>&2
      $echo "$help" 1>&2
      exit 1
    fi
    if test -n "$vinfo" && test -n "$release"; then
      $echo "$modename: you cannot specify both \`-version-info' and \`-release'" 1>&2
      $echo "$help" 1>&2
      exit 1
    fi
    oldlib=
    oldobjs=
    case "$output" in
    "")
      $echo "$modename: you must specify an output file" 1>&2
      $echo "$help" 1>&2
      exit 1
      ;;
    */* | *\\*)
      $echo "$modename: output file \`$output' must have no directory components" 1>&2
      exit 1
      ;;
    *.a)
      build_libtool_libs=no
      build_old_libs=yes
      oldlib="$output"
      $show "$rm $oldlib"
      $run $rm $oldlib
      ;;
    *.la)
      case "$output" in
      lib*) ;;
      *)
	$echo "$modename: libtool library \`$arg' must begin with \`lib'" 1>&2
	$echo "$help" 1>&2
	exit 1
	;;
      esac
      name=`$echo "X$output" | $Xsed -e 's/\.la$//' -e 's/^lib//'`
      eval libname=\"$libname_spec\"
      library_names=
      old_library=
      dlname=
      current=0
      revision=0
      age=0
      if test -n "$objs"; then
        $echo "$modename: cannot build libtool library \`$output' from non-libtool objects:$objs" 2>&1
        exit 1
      fi
      if test -n "$link_against_libtool_libs"; then
        $echo "$modename: libtool library \`$output' may not depend on uninstalled libraries:$link_against_libtool_libs" 1>&2
        exit 1
      fi
      if test -n "$dlfiles$dlprefiles"; then
        $echo "$modename: warning: \`-dlopen' is ignored while creating libtool libraries" 1>&2
        compile_command=`$echo "X$compile_command" | $Xsed -e "s% @SYMFILE@%%"`
        finalize_command=`$echo "X$finalize_command" | $Xsed -e "s% @SYMFILE@%%"`
      fi
      if test -z "$rpath"; then
        $echo "$modename: you must specify an installation directory with \`-rpath'" 1>&2
	$echo "$help" 1>&2
        exit 1
      fi
      set dummy $rpath
      if test $
	$echo "$modename: warning: ignoring multiple \`-rpath's for a libtool library" 1>&2
      fi
      install_libdir="$2"
      IFS="${IFS= 	}"; save_ifs="$IFS"; IFS=':'
      set dummy $vinfo
      IFS="$save_ifs"
      if test -n "$5"; then
        $echo "$modename: too many parameters to \`-version-info'" 1>&2
        $echo "$help" 1>&2
        exit 1
      fi
      test -n "$2" && current="$2"
      test -n "$3" && revision="$3"
      test -n "$4" && age="$4"
      case "$current" in
      0 | [1-9] | [1-9][0-9]*) ;;
      *)
        $echo "$modename: CURRENT \`$current' is not a nonnegative integer" 1>&2
        $echo "$modename: \`$vinfo' is not valid version information" 1>&2
        exit 1
        ;;
      esac
      case "$revision" in
      0 | [1-9] | [1-9][0-9]*) ;;
      *)
        $echo "$modename: REVISION \`$revision' is not a nonnegative integer" 1>&2
        $echo "$modename: \`$vinfo' is not valid version information" 1>&2
        exit 1
        ;;
      esac
      case "$age" in
      0 | [1-9] | [1-9][0-9]*) ;;
      *)
        $echo "$modename: AGE \`$age' is not a nonnegative integer" 1>&2
        $echo "$modename: \`$vinfo' is not valid version information" 1>&2
        exit 1
        ;;
      esac
      if test $age -gt $current; then
        $echo "$modename: AGE \`$age' is greater than the current interface number \`$current'" 1>&2
        $echo "$modename: \`$vinfo' is not valid version information" 1>&2
        exit 1
      fi
      version_vars="version_type current age revision"
      case "$version_type" in
      none) ;;
      linux)
        version_vars="$version_vars major versuffix"
        major=`expr $current - $age`
        versuffix="$major.$age.$revision"
        ;;
      osf)
        version_vars="$version_vars versuffix verstring"
        major=`expr $current - $age`
        versuffix="$current.$age.$revision"
        verstring="$versuffix"
        loop=$age
        while test $loop != 0; do
          iface=`expr $current - $loop`
          loop=`expr $loop - 1`
          verstring="$verstring:${iface}.0"
        done
        verstring="$verstring:${current}.0"
        ;;
      sunos)
        version_vars="$version_vars major versuffix"
        major="$current"
        versuffix="$current.$revision"
        ;;
      *)
        $echo "$modename: unknown library version type \`$version_type'" 1>&2
        echo "Fatal configuration error.  See the $PACKAGE docs for more information." 1>&2
        exit 1
        ;;
      esac
      if test -d $objdir; then
        $show "$rm $objdir/$output $objdir/$libname.* $objdir/${libname}${release}.*"
        $run $rm $objdir/$output $objdir/$libname.* $objdir/${libname}${release}.*
      else
        $show "$mkdir $objdir"
        $run $mkdir $objdir
	status=$?
	if test $status -eq 0 || test -d $objdir; then :
	else
	  exit $status
	fi
      fi
      if test "$allow_undefined" = yes; then
        if test "$allow_undefined_flag" = unsupported; then
          $echo "$modename: warning: undefined symbols not allowed in $host shared libraries" 1>&2
          build_libtool_libs=no
	  build_old_libs=yes
        fi
      else
        allow_undefined_flag="$no_undefined_flag"
      fi
      dependency_libs="$deplibs"
      deplibs="$deplibs -lc"
      if test "$build_libtool_libs" = yes; then
        eval library_names=\"$library_names_spec\"
        set dummy $library_names
        realname="$2"
        shift; shift
        if test -n "$soname_spec"; then
          eval soname=\"$soname_spec\"
        else
          soname="$realname"
        fi
        lib="$objdir/$realname"
	for link
	do
	  linknames="$linknames $link"
	done
        test -z "$pic_flag" && libobjs=`$echo "X$libobjs " | $Xsed -e 's/\.lo /.o /g' -e 's/ $//g'`
        eval cmds=\"$archive_cmds\"
        IFS="${IFS= 	}"; save_ifs="$IFS"; IFS=';'
        for cmd in $cmds; do
          IFS="$save_ifs"
          $show "$cmd"
          $run eval "$cmd" || exit $?
        done
        IFS="$save_ifs"
        for linkname in $linknames; do
          $show "(cd $objdir && $LN_S $realname $linkname)"
          $run eval '(cd $objdir && $LN_S $realname $linkname)' || exit $?
        done
        if test "$export_dynamic" = yes; then
          dlname="$soname"
        fi
      fi
      oldlib="$objdir/$libname.a"
      ;;
    *.lo | *.o)
      if test -n "$link_against_libtool_libs"; then
        $echo "$modename: error: cannot link libtool libraries into reloadable objects" 1>&2
        exit 1
      fi
      if test -n "$deplibs"; then
        $echo "$modename: warning: \`-l' and \`-L' are ignored while creating objects" 1>&2
      fi
      if test -n "$dlfiles$dlprefiles"; then
        $echo "$modename: warning: \`-dlopen' is ignored while creating objects" 1>&2
        compile_command=`$echo "X$compile_command" | $Xsed -e "s% @SYMFILE@%%"`
        finalize_command=`$echo "X$finalize_command" | $Xsed -e "s% @SYMFILE@%%"`
      fi
      if test -n "$rpath"; then
        $echo "$modename: warning: \`-rpath' is ignored while creating objects" 1>&2
      fi
      if test -n "$vinfo"; then
        $echo "$modename: warning: \`-version-info' is ignored while creating objects" 1>&2
      fi
      if test -n "$release"; then
        $echo "$modename: warning: \`-release' is ignored while creating objects" 1>&2
      fi
      case "$output" in
      *.lo)
        if test -n "$objs"; then
          $echo "$modename: cannot build library object \`$output' from non-libtool objects" 1>&2
          exit 1
        fi
        libobj="$output"
        obj=`$echo "X$output" | $Xsed -e 's/\.lo$/.o/'`
        ;;
      *)
        libobj=
        obj="$output"
        ;;
      esac
      $run $rm $obj $libobj
      reload_objs="$objs"`$echo "X$libobjs " | $Xsed -e 's/[^       ]*\.a //g' -e 's/\.lo /.o /g' -e 's/ $//g'`
      output="$obj"
      eval cmds=\"$reload_cmds\"
      IFS="${IFS= 	}"; save_ifs="$IFS"; IFS=';'
      for cmd in $cmds; do
        IFS="$save_ifs"
        $show "$cmd"
        $run eval "$cmd" || exit $?
      done
      IFS="$save_ifs"
      test -z "$libobj" && exit 0
      if test "$build_libtool_libs" != yes; then
        $show "echo timestamp > $libobj"
        $run eval "echo timestamp > $libobj" || exit $?
        exit 0
      fi
      if test -n "$pic_flag"; then
        reload_objs="$libobjs"
        output="$libobj"
        eval cmds=\"$reload_cmds\"
        IFS="${IFS= 	}"; save_ifs="$IFS"; IFS=';'
        for cmd in $cmds; do
          IFS="$save_ifs"
          $show "$cmd"
          $run eval "$cmd" || exit $?
        done
        IFS="$save_ifs"
      else
        $show "$LN_S $obj $libobj"
        $run $LN_S $obj $libobj || exit 1
      fi
      exit 0
      ;;
    *)
      if test -n "$vinfo"; then
        $echo "$modename: warning: \`-version-info' is ignored while linking programs" 1>&2
      fi
      if test -n "$release"; then
        $echo "$modename: warning: \`-release' is ignored while creating objects" 1>&2
      fi
      if test -n "$rpath"; then
	for libdir in $rpath; do
          if test -n "$hardcode_libdir_flag_spec"; then
            if test -n "$hardcode_libdir_separator"; then
              if test -z "$hardcode_libdirs"; then
                hardcode_libdirs="$libdir"
                libdir="@HARDCODE_LIBDIRS@"
              else
		case "$hardcode_libdir_separator$hardcode_libdirs$hardcode_libdir_separator" in
		*"$hardcode_libdir_separator$libdir$hardcode_libdir_separator"*)
		  ;;
		*)
		  hardcode_libdirs="$hardcode_libdirs$hardcode_libdir_separator$libdir"
		  ;;
		esac
                libdir=
              fi
            fi
            if test -n "$libdir"; then
              eval flag=\"$hardcode_libdir_flag_spec\"
              compile_command="$compile_command $flag"
              finalize_command="$finalize_command $flag"
            fi
          elif test -n "$runpath_var"; then
            case "$perm_rpath " in
            *" $libdir "*) ;;
            *) perm_rpath="$perm_rpath $libdir" ;;
            esac
          fi
	done
      fi
      if test -n "$hardcode_libdir_separator"; then
	compile_command=`$echo "X$compile_command" | $Xsed -e "s%@HARDCODE_LIBDIRS@%$hardcode_libdirs%g"`
	finalize_command=`$echo "X$finalize_command" | $Xsed -e "s%@HARDCODE_LIBDIRS@%$hardcode_libdirs%g"`
      fi
      if test -n "$libobjs" && test "$build_old_libs" = yes; then
        compile_command=`$echo "X$compile_command " | $Xsed -e 's/\.lo /.o /g' -e 's/ $//'`
        finalize_command=`$echo "X$finalize_command " | $Xsed -e 's/\.lo /.o /g' -e 's/ $//'`
      fi
      if test "$export_dynamic" = yes && test -n "$NM" && test -n "$global_symbol_pipe"; then
        dlsyms="${output}S.c"
      else
        dlsyms=
      fi
      if test -n "$dlsyms"; then
        dlprefiles=`$echo "X$objs$dlprefiles " | $Xsed -e 's/\.lo /.o /g' -e 's/ $//'`
        nlist="$objdir/${output}.nm"
	if test -d $objdir; then
	  $show "$rm $nlist ${nlist}T"
	  $run $rm "$nlist" "${nlist}T"
	else
	  $show "$mkdir $objdir"
	  $run $mkdir $objdir
	  status=$?
	  if test $status -eq 0 || test -d $objdir; then :
	  else
	    exit $status
	  fi
	fi
        for arg in $dlprefiles; do
	  $show "extracting global C symbols from \`$arg'"
	  $run eval "$NM $arg | $global_symbol_pipe >> '$nlist'"
        done
        $show "creating $objdir/$dlsyms"
        if test -z "$run"; then
	  test -f "$nlist" || : > "$nlist"
	  if sort "$nlist" | uniq > "$nlist"T; then
	    mv -f "$nlist"T "$nlist"
	    wcout=`wc "$nlist" 2>/dev/null`
	    count=`echo "X$wcout" | $Xsed -e 's/^[ 	]*\([0-9][0-9]*\).*$/\1/'`
	    (test "$count" -ge 0) 2>/dev/null || count=-1
	  else
	    $rm "$nlist"T
	    count=-1
	  fi
	  case "$dlsyms" in
	  "") ;;
	  *.c)
	    $echo > "$objdir/$dlsyms" "\
/* $dlsyms - symbol resolution table for \`$output' dlsym emulation. */
/* Generated by $PROGRAM - GNU $PACKAGE $VERSION */
extern \"C\" {
/* Prevent the only kind of declaration conflicts we can make. */
/* External symbol declarations for the compiler. */\
"
	    if test -f "$nlist"; then
	      sed -e 's/^.* \(.*\)$/extern char \1;/' < "$nlist" >> "$objdir/$dlsyms"
	    else
	      echo '/* NONE */' >> "$objdir/$dlsyms"
	    fi
	    $echo >> "$objdir/$dlsyms" "\
/* The number of symbols in dld_preloaded_symbols, -1 if unsorted. */
int dld_preloaded_symbol_count = $count;
/* The mapping between symbol names and symbols. */
struct {
  char *name;
  __ptr_t address;
}
dld_preloaded_symbols[] =
{\
"
	    if test -f "$nlist"; then
	      sed 's/^\(.*\) \(.*\)$/  {"\1", (__ptr_t) \&\2},/' < "$nlist" >> "$objdir/$dlsyms"
	    fi
	    $echo >> "$objdir/$dlsyms" "\
  {0, (__ptr_t) 0}
};
}
"
	    ;;
	  *)
	    $echo "$modename: unknown suffix for \`$dlsyms'" 1>&2
	    exit 1
	    ;;
	  esac
        fi
        $show "(cd $objdir && $CC -c$no_builtin_flag \"$dlsyms\")"
        $run eval '(cd $objdir && $CC -c$no_builtin_flag "$dlsyms")' || exit $?
        compile_command=`$echo "X$compile_command" | $Xsed -e "s%@SYMFILE@%$objdir/${output}S.o%"`
        finalize_command=`$echo "X$finalize_command" | $Xsed -e "s%@SYMFILE@%$objdir/${output}S.o%"`
      elif test "$export_dynamic" != yes; then
        test -n "$dlfiles$dlprefiles" && $echo "$modename: warning: \`-dlopen' and \`-dlpreopen' are ignored without \`-export-dynamic'" 1>&2
      else
        $echo "$modename: not configured to extract global symbols from dlpreopened files" 1>&2
        compile_command=`$echo "X$compile_command" | $Xsed -e "s% @SYMFILE@%%"`
        finalize_command=`$echo "X$finalize_command" | $Xsed -e "s% @SYMFILE@%%"`
      fi
      if test -z "$link_against_libtool_libs" || test "$build_libtool_libs" != yes; then
        compile_command=`$echo "X$compile_command" | $Xsed -e 's%@OUTPUT@%'"$output"'%g'`
        finalize_command=`$echo "X$finalize_command" | $Xsed -e 's%@OUTPUT@%'"$output"'%g'`
        $show "$compile_command"
        $run eval "$compile_command"
        exit $?
      fi
      compile_command=`$echo "X$compile_command" | $Xsed -e 's%@OUTPUT@%'"$objdir/$output"'%g'`
      finalize_command=`$echo "X$finalize_command" | $Xsed -e 's%@OUTPUT@%'"$objdir/$output"'T%g'`
      if test -d $objdir; then :
      else
        $show "$mkdir $objdir"
	$run $mkdir $objdir
	status=$?
	if test $status -eq 0 || test -d $objdir; then :
	else
	  exit $status
	fi
      fi
      if test -n "$shlibpath_var"; then
        rpath=
        for dir in $temp_rpath; do
          case "$dir" in
          /* | [A-Za-z]:\\*)
            rpath="$rpath$dir:"
            ;;
          *)
            rpath="$rpath\$thisdir/$dir:"
            ;;
          esac
        done
        temp_rpath="$rpath"
      fi
      $run $rm $output
      if test -n "$compile_shlibpath"; then
        compile_command="$shlibpath_var=\"$compile_shlibpath\$$shlibpath_var\" $compile_command"
      fi
      if test -n "$finalize_shlibpath"; then
        finalize_command="$shlibpath_var=\"$finalize_shlibpath\$$shlibpath_var\" $finalize_command"
      fi
      if test -n "$runpath_var" && test -n "$perm_rpath"; then
        rpath=
        for dir in $perm_rpath; do
          rpath="$rpath$dir:"
        done
        compile_command="$runpath_var=\"$rpath\$$runpath_var\" $compile_command"
        finalize_command="$runpath_var=\"$rpath\$$runpath_var\" $finalize_command"
      fi
      case "$hardcode_action" in
      relink)
        $echo "$modename: warning: using a buggy system linker" 1>&2
        $echo "$modename: relinking will be required before \`$output' can be installed" 1>&2
        ;;
      esac
      $show "$compile_command"
      $run eval "$compile_command" || exit $?
      $show "creating $output"
      finalize_command=`$echo "X$finalize_command" | $Xsed -e "$sed_quote_subst"`
      qecho=`$echo "X$echo" | $Xsed -e "$sed_quote_subst"`
      if test -z "$run"; then
        $rm $output
        trap "$rm $output; exit 1" 1 2 15
        $echo > $output "\
Xsed='sed -e s/^X//'
sed_quote_subst='$sed_quote_subst'
if test \"\${CDPATH+set}\" = set; then CDPATH=; export CDPATH; fi
if test \"\$libtool_install_magic\" = \"$magic\"; then
  link_against_libtool_libs='$link_against_libtool_libs'
  finalize_command=\"$finalize_command\"
else
  if test \"\$libtool_execute_magic\" = \"$magic\"; then :
  else
    echo=\"$qecho\"
    file=\"\$0\"
  fi\
"
        $echo >> $output "\
  thisdir=\`\$echo \"X\$file\" | \$Xsed -e 's%/[^/]*$%%'\`
  test \"x\$thisdir\" = \"x\$file\" && thisdir=.
  file=\`ls -ld \"\$file\" | sed -n 's/.*-> //p'\`
  while test -n \"\$file\"; do
    destdir=\`\$echo \"X\$file\" | \$Xsed -e 's%/[^/]*\$%%'\`
    if test \"x\$destdir\" != \"x\$file\"; then
      case \"\$destdir\" in
      /* | [A-Za-z]:\\*) thisdir=\"\$destdir\" ;;
      *) thisdir=\"\$thisdir/\$destdir\" ;;
      esac
    fi
    file=\`\$echo \"X\$file\" | \$Xsed -e 's%^.*/%%'\`
    file=\`ls -ld \"\$thisdir/\$file\" | sed -n 's/.*-> //p'\`
  done
  absdir=\`cd \"\$thisdir\" && pwd\`
  test -n \"\$absdir\" && thisdir=\"\$absdir\"
  progdir=\"\$thisdir/$objdir\"
  program='$output'
  if test -f \"\$progdir/\$program\"; then"
        if test -n "$shlibpath_var" && test -n "$temp_rpath"; then
          $echo >> $output "\
    $shlibpath_var=\"$temp_rpath\$$shlibpath_var\"
    $shlibpath_var=\`\$echo \"X\$$shlibpath_var\" | \$Xsed -e 's/:*\$//'\`
    export $shlibpath_var
"
        fi
        $echo >> $output "\
    if test \"\$libtool_execute_magic\" != \"$magic\"; then
      PATH=\"\$progdir:\$PATH\"
      export PATH
      exec \$program \${1+\"\$@\"}
      \$echo \"\$0: cannot exec \$program \${1+\"\$@\"}\"
      exit 1
    fi
  else
    \$echo \"\$0: error: \$progdir/\$program does not exist\" 1>&2
    \$echo \"This script is just a wrapper for \$program.\" 1>&2
    echo \"See the $PACKAGE documentation for more information.\" 1>&2
    exit 1
  fi
fi\
"
        chmod +x $output
      fi
      exit 0
      ;;
    esac
    if test "$build_old_libs" = "yes"; then
      oldobjs="$objs"`$echo "X$libobjs " | $Xsed -e 's/[^   ]*\.a //g' -e 's/\.lo /.o /g' -e 's/ $//g'`
      if test -n "$old_archive_from_new_cmds" && test "$build_libtool_libs" = yes; then
	eval cmds=\"$old_archive_from_new_cmds\"
      else
	eval cmds=\"$old_archive_cmds\"
      fi
      IFS="${IFS= 	}"; save_ifs="$IFS"; IFS=';'
      for cmd in $cmds; do
        IFS="$save_ifs"
        $show "$cmd"
        $run eval "$cmd" || exit $?
      done
      IFS="$save_ifs"
    fi
    case "$output" in
    *.la)
      old_library=
      test "$build_old_libs" = yes && old_library="$libname.a"
      $show "creating $output"
      if test -z "$run"; then
        $echo > $output "\
dlname='$dlname'
library_names='$library_names'
old_library='$old_library'
dependency_libs='$dependency_libs'
current=$current
age=$age
revision=$revision
libdir='$install_libdir'\
"
      fi
      $show "(cd $objdir && $LN_S ../$output $output)"
      $run eval "(cd $objdir && $LN_S ../$output $output)" || exit 1
      ;;
    esac
    exit 0
    ;;
  install)
    modename="$modename: install"
    if test "$nonopt" = "$SHELL"; then
      arg=`$echo "X$nonopt" | $Xsed -e "$sed_quote_subst"`
      case "$arg" in
      *[\[\~\
	arg="\"$arg\""
	;;
      esac
      install_prog="$arg "
      arg="$1"
      shift
    else
      install_prog=
      arg="$nonopt"
    fi
    arg=`$echo "X$arg" | $Xsed -e "$sed_quote_subst"`
    case "$arg" in
    *[\[\~\
      arg="\"$arg\""
      ;;
    esac
    install_prog="$install_prog$arg"
    dest=
    files=
    opts=
    prev=
    install_type=
    isdir=
    stripme=
    for arg
    do
      if test -n "$dest"; then
        files="$files $dest"
        dest="$arg"
        continue
      fi
      case "$arg" in
      -d) isdir=yes ;;
      -f) prev="-f" ;;
      -g) prev="-g" ;;
      -m) prev="-m" ;;
      -o) prev="-o" ;;
      -s)
        stripme=" -s"
        continue
        ;;
      -*) ;;
      *)
        if test -n "$prev"; then
          prev=
        else
          dest="$arg"
          continue
        fi
        ;;
      esac
      arg=`$echo "X$arg" | $Xsed -e "$sed_quote_subst"`
      case "$arg" in
      *[\[\~\
	arg="\"$arg\""
	;;
      esac
      install_prog="$install_prog $arg"
    done
    if test -z "$install_prog"; then
      $echo "$modename: you must specify an install program" 1>&2
      $echo "$help" 1>&2
      exit 1
    fi
    if test -n "$prev"; then
      $echo "$modename: the \`$prev' option requires an argument" 1>&2
      $echo "$help" 1>&2
      exit 1
    fi
    if test -z "$files"; then
      if test -z "$dest"; then
        $echo "$modename: no file or destination specified" 1>&2
      else
        $echo "$modename: you must specify a destination" 1>&2
      fi
      $echo "$help" 1>&2
      exit 1
    fi
    dest=`$echo "X$dest" | $Xsed -e 's%/$%%'`
    test -d "$dest" && isdir=yes
    if test -n "$isdir"; then
      destdir="$dest"
      destname=
    else
      destdir=`$echo "X$dest" | $Xsed -e 's%/[^/]*$%%'`
      test "X$destdir" = "X$dest" && destdir=.
      destname=`$echo "X$dest" | $Xsed -e 's%^.*/%%'`
      set dummy $files
      if test $
        $echo "$modename: \`$dest' is not a directory" 1>&2
        $echo "$help" 1>&2
        exit 1
      fi
    fi
    case "$destdir" in
    /* | [A-Za-z]:\\*) ;;
    *)
      for file in $files; do
        case "$file" in
        *.lo) ;;
        *)
          $echo "$modename: \`$destdir' must be an absolute directory name" 1>&2
          $echo "$help" 1>&2
          exit 1
          ;;
        esac
      done
      ;;
    esac
    libtool_install_magic="$magic"
    staticlibs=
    future_libdirs=
    current_libdirs=
    for file in $files; do
      case "$file" in
      *.a)
        staticlibs="$staticlibs $file"
        ;;
      *.la)
        if (sed -e '2q' $file | egrep '^
        else
          $echo "$modename: \`$file' is not a valid libtool archive" 1>&2
          $echo "$help" 1>&2
          exit 1
        fi
        library_names=
        old_library=
        case "$file" in
        */* | *\\*) . $file ;;
        *) . ./$file ;;
        esac
        if test "X$destdir" = "X$libdir"; then
          case "$current_libdirs " in
          *" $libdir "*) ;;
          *) current_libdirs="$current_libdirs $libdir" ;;
          esac
        else
          case "$future_libdirs " in
          *" $libdir "*) ;;
          *) future_libdirs="$future_libdirs $libdir" ;;
          esac
        fi
        dir="`$echo "X$file" | $Xsed -e 's%/[^/]*$%%'`/"
        test "X$dir" = "X$file/" && dir=
        dir="$dir$objdir"
        set dummy $library_names
        if test -n "$2"; then
          realname="$2"
          shift
          shift
          $show "$install_prog $dir/$realname $destdir/$realname"
          $run eval "$install_prog $dir/$realname $destdir/$realname" || exit $?
          test "X$dlname" = "X$realname" && dlname=
          if test $
            rmcmd="$rm"
            for linkname
            do
              rmcmd="$rmcmd $destdir/$linkname"
            done
            $show "$rmcmd"
            $run $rmcmd
            for linkname
            do
              test "X$dlname" = "X$linkname" && dlname=
              $show "(cd $destdir && $LN_S $realname $linkname)"
              $run eval "(cd $destdir && $LN_S $realname $linkname)"
            done
          fi
          if test -n "$dlname"; then
            $show "$install_prog $dir/$dlname $destdir/$dlname"
            $run eval "$install_prog $dir/$dlname $destdir/$dlname" || exit $?
          fi
          lib="$destdir/$realname"
          eval cmds=\"$postinstall_cmds\"
          IFS="${IFS= 	}"; save_ifs="$IFS"; IFS=';'
          for cmd in $cmds; do
            IFS="$save_ifs"
            $show "$cmd"
            $run eval "$cmd" || exit $?
          done
          IFS="$save_ifs"
        fi
        name=`$echo "X$file" | $Xsed -e 's%^.*/%%'`
        $show "$install_prog $file $destdir/$name"
        $run eval "$install_prog $file $destdir/$name" || exit $?
        test -n "$old_library" && staticlibs="$staticlibs $dir/$old_library"
        ;;
      *.lo)
        if test -n "$destname"; then
          destfile="$destdir/$destname"
        else
          destfile=`$echo "X$file" | $Xsed -e 's%^.*/%%'`
          destfile="$destdir/$destfile"
        fi
        case "$destfile" in
        *.lo)
          staticdest=`$echo "X$destfile" | $Xsed -e 's/\.lo$/\.o/'`
          ;;
        *.o)
          staticdest="$destfile"
          destfile=
          ;;
        *)
          $echo "$modename: cannot copy a libtool object to \`$destfile'" 1>&2
          $echo "$help" 1>&2
          exit 1
          ;;
        esac
        if test -n "$destfile"; then
          $show "$install_prog $file $destfile"
          $run eval "$install_prog $file $destfile" || exit $?
        fi
        if test "$build_old_libs" = yes; then
          staticobj=`$echo "X$file" | $Xsed -e 's/\.lo$/\.o/'`
          $show "$install_prog $staticobj $staticdest"
          $run eval "$install_prog \$staticobj \$staticdest" || exit $?
        fi
        exit 0
        ;;
      *)
        if (sed -e '4q' $file | egrep '^
          link_against_libtool_libs=
          finalize_command=
          case "$file" in
          */* | *\\*) . $file ;;
          *) . ./$file ;;
          esac
          if test -z "$link_against_libtool_libs" || test -z "$finalize_command"; then
            $echo "$modename: invalid libtool wrapper script \`$file'" 1>&2
            exit 1
          fi
          finalize=yes
          for lib in $link_against_libtool_libs; do
            libdir=
            if test -f "$lib"; then
              case "$lib" in
              */* | *\\*) . $lib ;;
              *) . ./$lib ;;
              esac
            fi
            libfile="$libdir/`$echo "X$lib" | $Xsed -e 's%^.*/%%g'`"
            if test -z "$libdir"; then
              $echo "$modename: warning: \`$lib' contains no -rpath information" 1>&2
            elif test -f "$libfile"; then :
            else
              $echo "$modename: warning: \`$lib' has not been installed in \`$libdir'" 1>&2
              finalize=no
            fi
          done
          if test "$hardcode_action" = relink; then
            if test "$finalize" = yes; then
              $echo "$modename: warning: relinking \`$file' on behalf of your buggy system linker" 1>&2
              $show "$finalize_command"
              if $run eval "$finalize_command"; then :
              else
                $echo "$modename: error: relink \`$file' with the above command before installing it" 1>&2
                continue
              fi
              file="$objdir/$file"T
            else
              $echo "$modename: warning: cannot relink \`$file' on behalf of your buggy system linker" 1>&2
            fi
          else
	    file=`$echo "X$file" | $Xsed -e "s%\([^/]*\)$%$objdir/\1%"`
          fi
        fi
        $show "$install_prog$stripme $file $dest"
        $run eval "$install_prog\$stripme \$file \$dest" || exit $?
        ;;
      esac
    done
    for file in $staticlibs; do
      name=`$echo "X$file" | $Xsed -e 's%^.*/%%'`
      oldlib="$destdir/$name"
      $show "$install_prog $file $oldlib"
      $run eval "$install_prog \$file \$oldlib" || exit $?
      eval cmds=\"$old_postinstall_cmds\"
      IFS="${IFS= 	}"; save_ifs="$IFS"; IFS=';'
      for cmd in $cmds; do
        IFS="$save_ifs"
        $show "$cmd"
        $run eval "$cmd" || exit $?
      done
      IFS="$save_ifs"
    done
    if test -n "$future_libdirs"; then
      $echo "$modename: warning: remember to run \`$progname --finish$future_libdirs'" 1>&2
    fi
    if test -n "$current_libdirs"; then
      test -n "$run" && current_libdirs=" -n$current_libdirs"
      exec $SHELL $0 --finish$current_libdirs
      exit 1
    fi
    exit 0
    ;;
  finish)
    modename="$modename: finish"
    libdirs="$nonopt"
    if test -n "$finish_cmds$finish_eval" && test -n "$libdirs"; then
      for dir
      do
        libdirs="$libdirs $dir"
      done
      for libdir in $libdirs; do
	if test -n "$finish_cmds"; then
	  eval cmds=\"$finish_cmds\"
          IFS="${IFS= 	}"; save_ifs="$IFS"; IFS=';'
          for cmd in $cmds; do
            IFS="$save_ifs"
            $show "$cmd"
            $run eval "$cmd"
          done
          IFS="$save_ifs"
	fi
	if test -n "$finish_eval"; then
	  eval cmds=\"$finish_eval\"
	  $run eval "$cmds"
	fi
      done
    fi
    echo "------------------------------------------------------------------------------"
    echo "Libraries have been installed in:"
    for libdir in $libdirs; do
      echo "   $libdir"
    done
    echo
    echo "To link against installed libraries in a given directory, LIBDIR,"
    echo "you must use the \`-LLIBDIR' flag during linking."
    echo
    echo " You will also need to do one of the following:"
    if test -n "$shlibpath_var"; then
      echo "   - add LIBDIR to the \`$shlibpath_var' environment variable"
      echo "     during execution"
    fi
    if test -n "$runpath_var"; then
      echo "   - add LIBDIR to the \`$runpath_var' environment variable"
      echo "     during linking"
    fi
    if test -n "$hardcode_libdir_flag_spec"; then
      libdir=LIBDIR
      eval flag=\"$hardcode_libdir_flag_spec\"
      echo "   - use the \`$flag' linker flag"
    fi
    if test -f /etc/ld.so.conf; then
      echo "   - have your system administrator add LIBDIR to \`/etc/ld.so.conf'"
    fi
    echo
    echo "See any operating system documentation about shared libraries for"
    echo "more information, such as the ld(1) and ld.so(8) manual pages."
    echo "------------------------------------------------------------------------------"
    exit 0
    ;;
  execute)
    modename="$modename: execute"
    cmd="$nonopt"
    if test -z "$cmd"; then
      $echo "$modename: you must specify a COMMAND" 1>&2
      $echo "$help"
      exit 1
    fi
    for file in $execute_dlfiles; do
      if test -f "$file"; then :
      else
	$echo "$modename: \`$file' is not a file" 1>&2
	$echo "$help" 1>&2
	exit 1
      fi
      dir=
      case "$file" in
      *.la)
        if (sed -e '2q' $file | egrep '^
        else
          $echo "$modename: \`$lib' is not a valid libtool archive" 1>&2
          $echo "$help" 1>&2
          exit 1
        fi
	dlname=
	library_names=
	case "$file" in
	*/* | *\\*) . $file ;;
        *) . ./$file ;;
	esac
	if test -z "$dlname"; then
	  test -n "$library_names" && $echo "$modename: warning: \`$file' was not linked with \`-export-dynamic'"
	  continue
	fi
	dir=`$echo "X$file" | $Xsed -e 's%/[^/]*$%%'`
	test "X$dir" = "X$file" && dir=.
	if test -f "$dir/$objdir/$dlname"; then
	  dir="$dir/$objdir"
	else
	  $echo "$modename: cannot find \`$dlname' in \`$dir' or \`$dir/$objdir'" 1>&2
	  exit 1
	fi
	;;
      *.lo)
	dir=`$echo "X$file" | $Xsed -e 's%/[^/]*$%%'`
	test "X$dir" = "X$file" && dir=.
	;;
      *)
	$echo "$modename: warning \`-dlopen' is ignored for non-libtool libraries and objects" 1>&2
        continue
	;;
      esac
      absdir=`cd "$dir" && pwd`
      test -n "$absdir" && dir="$absdir"
      if eval "test -z \"\$$shlibpath_var\""; then
	eval "$shlibpath_var=\"\$dir\""
      else
	eval "$shlibpath_var=\"\$dir:\$$shlibpath_var\""
      fi
    done
    libtool_execute_magic="$magic"
    args=
    for file
    do
      case "$file" in
      -*) ;;
      *)
        if (sed -e '4q' $file | egrep '^
	  case "$file" in
	  */* | *\\*) . $file ;;
	  *) . ./$file ;;
	  esac
	  file="$progdir/$program"
	fi
        ;;
      esac
      file=`$echo "X$file" | $Xsed -e "$sed_quote_subst"`
      args="$args \"$file\""
    done
    if test -z "$run"; then
      eval "export $shlibpath_var"
      eval "exec \$cmd$args"
      $echo "$modename: cannot exec \$cmd$args"
      exit 1
    else
      eval "\$echo \"\$shlibpath_var=\$$shlibpath_var\""
      $echo "export $shlibpath_var"
      $echo "$cmd$args"
      exit 0
    fi
    ;;
  uninstall)
    modename="$modename: uninstall"
    rm="$nonopt"
    files=
    for arg
    do
      case "$arg" in
      -*) rm="$rm $arg" ;;
      *) files="$files $arg" ;;
      esac
    done
    if test -z "$rm"; then
      $echo "$modename: you must specify an RM program" 1>&2
      $echo "$help" 1>&2
      exit 1
    fi
    for file in $files; do
      dir=`$echo "X$file" | $Xsed -e 's%/[^/]*$%%'`
      test "X$dir" = "X$file" && dir=.
      name=`$echo "X$file" | $Xsed -e 's%^.*/%%'`
      rmfiles="$file"
      case "$name" in
      *.la)
        if (sed -e '2q' $file | egrep '^
          . $dir/$name
          for n in $library_names; do
            rmfiles="$rmfiles $dir/$n"
            test "X$n" = "X$dlname" && dlname=
          done
          test -n "$dlname" && rmfiles="$rmfiles $dir/$dlname"
          test -n "$old_library" && rmfiles="$rmfiles $dir/$old_library"
	  $show "$rm $rmfiles"
	  $run $rm $rmfiles
	  if test -n "$library_names"; then
	    eval cmds=\"$postuninstall_cmds\"
	    IFS="${IFS= 	}"; save_ifs="$IFS"; IFS=';'
	    for cmd in $cmds; do
	      IFS="$save_ifs"
	      $show "$cmd"
	      $run eval "$cmd"
	    done
	    IFS="$save_ifs"
	  fi
          if test -n "$old_library"; then
	    eval cmds=\"$old_postuninstall_cmds\"
	    IFS="${IFS= 	}"; save_ifs="$IFS"; IFS=';'
	    for cmd in $cmds; do
	      IFS="$save_ifs"
	      $show "$cmd"
	      $run eval "$cmd"
	    done
	    IFS="$save_ifs"
	  fi
        fi
        ;;
      *.lo)
        if test "$build_old_libs" = yes; then
          oldobj=`$echo "X$name" | $Xsed -e 's/\.lo$/\.o/'`
          rmfiles="$rmfiles $dir/$oldobj"
        fi
	$show "$rm $rmfiles"
	$run $rm $rmfiles
        ;;
      *)
      	$show "$rm $rmfiles"
	$run $rm $rmfiles
	;;
      esac
    done
    exit 0
    ;;
  "")
    $echo "$modename: you must specify a MODE" 1>&2
    $echo "$generic_help" 1>&2
    exit 1
    ;;
  esac
  $echo "$modename: invalid operation mode \`$mode'" 1>&2
  $echo "$generic_help" 1>&2
  exit 1
fi
case "$mode" in
"") $echo \
"Usage: $modename [OPTION]... [MODE-ARG]...
Provide generalized library-building support services.
-n, --dry-run         display commands without modifying any files
    --features        display configuration information and exit
    --finish          same as \`--mode=finish'
    --help            display this help message and exit
    --mode=MODE       use operation mode MODE [default=inferred from MODE-ARGS]
    --quiet           same as \`--silent'
    --silent          don't print informational messages
    --version         print version information
MODE must be one of the following:
      compile         compile a source file into a libtool object
      execute         automatically set library path, then run a program
      finish          complete the installation of libtool libraries
      install         install libraries or executables
      link            create a library or an executable
      uninstall       remove libraries from an installed directory
MODE-ARGS vary depending on the MODE.  Try \`$modename --help --mode=MODE' for
a more detailed description of MODE."
  exit 0
  ;;
compile)
  $echo \
"Usage: $modename [OPTION]... --mode=compile COMPILE-COMMAND... SOURCEFILE
Compile a source file into a libtool library object.
COMPILE-COMMAND is a command to be used in creating a \`standard' object file
from the given SOURCEFILE.
The output file name is determined by removing the directory component from
SOURCEFILE, then substituting the C source code suffix \`.c' with the
library object suffix, \`.lo'."
  ;;
execute)
  $echo \
"Usage: $modename [OPTION]... --mode=execute COMMAND [ARGS]...
Automatically set library path, then run a program.
This mode accepts the following additional options:
  -dlopen FILE      add the directory containing FILE to the library path
This mode sets the library path environment variable according to \`-dlopen'
flags.
If any of the ARGS are libtool executable wrappers, then they are translated
into their corresponding uninstalled binary, and any of their required library
directories are added to the library path.
Then, COMMAND is executed, with ARGS as arguments."
  ;;
finish)
  $echo \
"Usage: $modename [OPTION]... --mode=finish [LIBDIR]...
Complete the installation of libtool libraries.
Each LIBDIR is a directory that contains libtool libraries.
The commands that this mode executes may require superuser privileges.  Use
the \`--dry-run' option if you just want to see what would be executed."
  ;;
install)
  $echo \
"Usage: $modename [OPTION]... --mode=install INSTALL-COMMAND...
Install executables or libraries.
INSTALL-COMMAND is the installation command.  The first component should be
either the \`install' or \`cp' program.
The rest of the components are interpreted as arguments to that command (only
BSD-compatible install options are recognized)."
  ;;
link)
  $echo \
"Usage: $modename [OPTION]... --mode=link LINK-COMMAND...
Link object files or libraries together to form another library, or to
create an executable program.
LINK-COMMAND is a command using the C compiler that you would use to create
a program from several object files.
The following components of LINK-COMMAND are treated specially:
  -all-static       do not do any dynamic linking at all
  -dlopen FILE      \`-dlpreopen' FILE if it cannot be dlopened at runtime
  -dlpreopen FILE   link in FILE and add its symbols to dld_preloaded_symbols
  -export-dynamic   allow symbols from OUTPUT-FILE to be resolved with dlsym(3)
  -LLIBDIR          search LIBDIR for required installed libraries
  -lNAME            OUTPUT-FILE requires the installed library libNAME
  -no-undefined     declare that a library does not refer to external symbols
  -o OUTPUT-FILE    create OUTPUT-FILE from the specified objects
  -release RELEASE  specify package release information
  -rpath LIBDIR     the created library will eventually be installed in LIBDIR
  -static           do not do any dynamic linking of libtool libraries
  -version-info CURRENT[:REVISION[:AGE]]
                    specify library version info [each variable defaults to 0]
All other options (arguments beginning with \`-') are ignored.
Every other argument is treated as a filename.  Files ending in \`.la' are
treated as uninstalled libtool libraries, other files are standard or library
object files.
If the OUTPUT-FILE ends in \`.la', then a libtool library is created, only
library objects (\`.lo' files) may be specified, and \`-rpath' is required.
If OUTPUT-FILE ends in \`.a', then a standard library is created using \`ar'
and \`ranlib'.
If OUTPUT-FILE ends in \`.lo' or \`.o', then a reloadable object file is
created, otherwise an executable program is created."
  ;;
uninstall)
  $echo
"Usage: $modename [OPTION]... --mode=uninstall RM [RM-OPTION]... FILE...
Remove libraries from an installation directory.
RM is the name of the program to use to delete files associated with each FILE
(typically \`/bin/rm').  RM-OPTIONS are options (such as \`-f') to be passed
to RM.
If FILE is a libtool library, all the files associated with it are deleted.
Otherwise, only FILE itself is deleted using RM."
  ;;
*)
  $echo "$modename: invalid operation mode \`$mode'" 1>&2
  $echo "$help" 1>&2
  exit 1
  ;;
esac
echo
$echo "Try \`$modename --help' for more information about other modes."
exit 0