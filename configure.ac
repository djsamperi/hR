AC_INIT([Haskell R interface], [0.1], [dylan@dylex.net])
AC_CONFIG_SRCDIR([hR.cabal])

AC_ARG_WITH(compiler, [AC_HELP_STRING([--with-compiler=hc], [use specified haskell compiler (ignored)])])
AC_ARG_WITH(gcc, [AC_HELP_STRING([--with-gcc=gcc], [use specified C compiler])], [CC=$withval])

AC_ARG_WITH(R,
	    [AC_HELP_STRING([--with-R=R], [use specified R @<:@default=check@:>@])],
	    [],
	    [with_R=yes])

AS_IF([test "$with_R" = yes],
      [AC_PATH_PROG([R_BIN], [R])],
      [test "$with_R" != no],
      [R_BIN="$with_R"])
AS_IF([test -z "$R_BIN"], [AC_MSG_ERROR([R is required])])
AC_MSG_CHECKING([R_HOME])

R_HOME=`env -i $R_BIN RHOME 2>&AS_MESSAGE_LOG_FD`
r_homeres=$?

AS_IF([test $r_homeres -gt 0 -o -z "$R_HOME"], 
      [AC_MSG_RESULT([failed])
      AC_MSG_FAILURE(["$R_BIN RHOME" failed])],
      [AC_MSG_RESULT([$R_HOME])])

PKG_CHECK_MODULES([R], [libR])
_PKG_CONFIG([R_LIBDIR], [variable=rlibdir], [libR])
AS_IF([test -n "$pkg_cv_R_LIBDIR"],
      [R_LIBS="$R_LIBS -Wl,-rpath=$pkg_cv_R_LIBDIR"])

LIBS="$LIBS $R_LIBS"
CFLAGS="$CFLAGS $R_CFLAGS"

AC_CHECK_FUNCS(R_SetErrorHook)

AC_SUBST([R_HOME])
AC_SUBST([R_LIBS])
AC_SUBST([R_CFLAGS])

AC_CONFIG_FILES([hR.buildinfo Foreign/R/Config.hs])
AC_CONFIG_HEADERS([src/config.h])
AC_OUTPUT
