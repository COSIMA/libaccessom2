
.PHONY: build

core:
	build/build.sh core
jra55:
	build/build.sh jra55

access-om2-025:
	build/build_gfdl025.sh

access-om2-025-scorep:
	build/build_gfdl025-scorep.sh
