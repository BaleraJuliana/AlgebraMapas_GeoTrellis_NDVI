package tutorial

import geotrellis.raster._
import geotrellis.raster.io.geotiff._
import geotrellis.raster.render._
import com.typesafe.config.ConfigFactory
import geotrellis.raster.mapalgebra.zonal._
import geotrellis.raster.mapalgebra.focal._
import scala.math._
import geotrellis.raster.summary._
import geotrellis.vector.Extent
import scala.collection.mutable
import geotrellis.spark.mapalgebra.local
import collection.mutable.ListBuffer


object anomalias {

	println("")
	println("")
	println(Console.GREEN+"+---------------------------------------------------------------------------------------+")
	println(Console.GREEN+"|"+Console.GREEN+"\t	AUXÍLIO A IDENTIFICAÇÃO DE ANOMALIAS DE VEGETAÇÃO :) 	\t\t"+Console.GREEN+"|")
	//println("|\t									     \t|")
	println("+---------------------------------------------------------------------------------------+")
	println(Console.GREEN+"|"+Console.GREEN+"\tPodemos realizar os seguintes tipos de operações:  		     	\t"+Console.GREEN+"|")
        println(Console.GREEN+"|\t "+Console.RED+"[1] média		      				                \t"+Console.GREEN+"|")									
	println(Console.GREEN+"|\t "+Console.CYAN+"[2] mínimo					                        \t"+Console.GREEN
+"|")	
	println(Console.GREEN+"|\t "+Console.YELLOW+"[3] máximo					                        \t"+Console.GREEN
+"|")	
	println("+---------------------------------------------------------------------------------------+")
	println("")
	println("")
	var escolha = readLine(Console.WHITE+"Digite o número da operação: "+Console.WHITE+"")
	var escolhas_validas = Set("1", "2", "3")
	while(!(escolhas_validas contains escolha))
	{
		println(" ")
		println(Console.RED + s">>>>> Não temos essa operação :( a operação ${escolha} não faz parte da nossa base de dados !<<<<<")
		escolha = readLine(Console.WHITE+"Digite a operação, novamente: "+Console.WHITE+"")
			
	}	

	println(Console.WHITE)
	

	def main(args: Array[String]): Unit = {
		
		val lista_geotiffs = ListBuffer[(SinglebandGeoTiff)]()
		println(Console.CYAN+"Lendo os dados..."+Console.WHITE)		
		for(a<-1 to 9)
		{
			println(a)
			val p1 = SinglebandGeoTiff(s"data/anomalias/landsat/DadosTratados/LC80140322014139LGN00_B${a}.TIF")
			println(s"Lendo dado LC80140322014139LGN00_B${a}.TIF")
			lista_geotiffs++= List(p1)
		}
		
		println(Console.YELLOW+"Leitura finalizada :)"+Console.WHITE)
		
				
		
		if(escolha=="1"){
			println("Somando GeoTiffs...")
			var media_geotiffs = lista_geotiffs(0).tile
			for(a<-0 to 9)
			{
				media_geotiffs = media_geotiffs + lista_geotiffs(a).tile
			}
			media_geotiffs = media_geotiffs.mapDouble(x => x/10)
			val mb = ArrayMultibandTile(media_geotiffs).convert(DoubleConstantNoDataCellType)
			println(Console.CYAN+"Gerando mapa com as médias NDVI..."+Console.WHITE+"")
    			MultibandGeoTiff(mb, lista_geotiffs(0).extent, lista_geotiffs(0).crs).write(s"data/saida/mapa_media_NDVI.tif")
		}
		
		if(escolha=="2"){
			
			val linhas = lista_geotiffs(0).tile.rows
			val colunas = lista_geotiffs(0).tile.cols
			var cellType = lista_geotiffs(0).tile.cellType
			val minimo_geotiffs = ArrayTile.empty(cellType, colunas, linhas)
			
			
			for(i<-0 to linhas){
				for(j<-0 to colunas){
					var maior = 99999999
					for(loop<-0 to 9){
						if(lista_geotiffs(loop).tile.get(i,j)<maior){
							//println(lista_geotiffs(loop).tile.get(i,j))
							maior = lista_geotiffs(loop).tile.get(i,j)
						}
					}
					minimo_geotiffs.setDouble(i,j,maior)
				}
			}

			val mb = ArrayMultibandTile(minimo_geotiffs).convert(DoubleConstantNoDataCellType)
			println(Console.CYAN+"Gerando mapa com mínimo NDVI..."+Console.WHITE+"")
    			MultibandGeoTiff(mb, lista_geotiffs(0).extent, lista_geotiffs(0).crs).write(s"data/saida/mapa_minima_NDVI.tif")
		}
		if(escolha=="3"){
			
			val linhas = lista_geotiffs(0).tile.rows
			val colunas = lista_geotiffs(0).tile.cols
			var cellType = lista_geotiffs(0).tile.cellType
			val maximo_geotiffs = ArrayTile.empty(cellType, colunas, linhas)
			
			
			for(i<-0 to linhas){
				for(j<-0 to colunas){
					var maior = -99999999 
					for(loop<-0 to 9){
						if(lista_geotiffs(loop).tile.get(i,j)>maior){
							maior = lista_geotiffs(loop).tile.get(i,j)
						}
					}
					maximo_geotiffs.setDouble(i,j,maior)
				}
			}

			val mb = ArrayMultibandTile(maximo_geotiffs).convert(DoubleConstantNoDataCellType)
			println(Console.CYAN+"Gerando mapa com máximo NDVI..."+Console.WHITE+"")
    			MultibandGeoTiff(mb, lista_geotiffs(0).extent, lista_geotiffs(0).crs).write(s"data/saida/mapa_maximo_NDVI.tif")
		}
		
		
	}
}
	
