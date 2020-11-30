cd $WorkSpace/2019-07-04_paleoCoastlines/data/PaleoMAP/2020-08-10_v7/



for i in $(ls CM/*.shp)
do
	gplates convert-file-format -l $i -e gpml
done

for i in $(ls CS/*.shp)
do
	gplates convert-file-format -l $i -e gpml
done
