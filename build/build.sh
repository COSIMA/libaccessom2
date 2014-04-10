#! /bin/csh -f

if ( $1 == '') then
  set platform = nci
else
  set platform = $1
endif

# Location of this model
setenv SRCDIR $cwd
setenv BLD $SRCDIR/build
setenv OASIS_HOME $cwd:h/coupler

# Set local setting such as loading modules. 
source $BLD/environs.$platform

### Location and names of coupling libraries
setenv CPLLIBDIR $OASIS_HOME/Linux/lib
setenv CPLLIBS '-L$(CPLLIBDIR) -lpsmile.MPI1 -lmct -lmpeu -lscrip'

### Location of coupling inclusions
setenv CPLINCDIR $OASIS_HOME/Linux/build/lib
setenv CPL_INCS '-I$(CPLINCDIR)/psmile.MPI1 -I$(CPLINCDIR)/pio -I$(CPLINCDIR)/mct'

### Grid resolution
setenv GRID nt62
setenv RES 192x94

### Location and name of the generated exectuable 
setenv EXE matm_${GRID}.exe

### Where this model is compiled
setenv OBJDIR $SRCDIR/build_${GRID}
if !(-d $OBJDIR) mkdir -p $OBJDIR

set NXGLOB = `echo $RES | sed s/x.\*//`
set NYGLOB = `echo $RES | sed s/.\*x//`

cp -f $BLD/Makefile.std $BLD/Makefile

cd $OBJDIR

### List of source code directories (in order of importance).
cat >! Filepath << EOF
$SRCDIR/source
EOF

cc -o makdep $BLD/makdep.c                      || exit 2

make VPFILE=Filepath EXEC=$EXE \
           NXGLOB=$NXGLOB NYGLOB=$NYGLOB \
      -f   $BLD/Makefile MACFILE=$BLD/Macros.$platform || exit 2

cd ..

