#
# This is the testing tool described in "Definitive ANTRL 4."
#

cd build/production/Merc
java -cp .:../../../../../../lib/antlr4-4.5.3.jar org.antlr.v4.runtime.misc.TestRig edu.vtc.merc.Main $1 $2 $3 $4
cd ../../../..
