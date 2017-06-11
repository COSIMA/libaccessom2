#! /bin/csh -f

set grid = $1
if ( $grid == '' ) then
    setenv GRID nt62
    setenv RES 192x94
endif
if ( $grid == 'core' ) then
    setenv GRID nt62
    setenv RES 192x94
endif
if ( $grid == 'jra55' ) then
    setenv GRID jra55
    setenv RES 640x320
endif

set platform = nci

set debug = $2
if ($debug == 'debug') then
    setenv DEBUG yes
endif

# Location of this model
setenv SRCDIR $cwd
setenv BLD $SRCDIR/build

# Set local setting such as loading modules. 
source $BLD/environs.$platform

### Location and names of coupling libraries
setenv CPLLIBDIR $OASIS_ROOT/Linux/lib
setenv CPLLIBS '-L$(CPLLIBDIR) -lpsmile.MPI1 -lmct -lmpeu -lscrip'

### Location of coupling inclusions
setenv CPLINCDIR $OASIS_ROOT/Linux/build/lib
setenv CPL_INCS '-I$(CPLINCDIR)/psmile.MPI1 -I$(CPLINCDIR)/pio -I$(CPLINCDIR)/mct'

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

