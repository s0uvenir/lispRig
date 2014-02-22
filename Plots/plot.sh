for i in `ls *.plt`; 
do
	python csvplot.py $i -s
done
