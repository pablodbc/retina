#!/bin/bash
make
rm resultados errores 2> /dev/null
touch resultados
touch errores
echo "Se han creado 2 archivos: resultados y errores"
for i in casos/lilue/*.rtn 
do 
	echo "$i" | tee -a resultados  errores > /dev/null
	echo | tee -a resultados  errores > /dev/null
	echo | tee -a resultados  errores > /dev/null
	cat "$i" | tee -a resultados errores > /dev/null
	echo | tee -a resultados  errores > /dev/null
	echo | tee -a resultados  errores > /dev/null
	echo 'INICIO DE ANALISIS (Si esta vacio, esta en el otro archivo)' | tee -a resultados  errores > /dev/null
	echo | tee -a resultados  errores > /dev/null
	echo | tee -a resultados  errores > /dev/null
	./retina "$i" >> resultados 2>> errores
	echo | tee -a resultados  errores > /dev/null
	echo | tee -a resultados  errores > /dev/null
	echo '###############################################################' | tee -a resultados  errores > /dev/null
	echo | tee -a resultados  errores > /dev/null
	echo | tee -a resultados  errores > /dev/null
done

