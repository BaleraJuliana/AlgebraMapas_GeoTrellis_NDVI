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


object NDVI {

	def main(args: Array[String]): Unit = {
		
		val lista_geotiffs_RED = ListBuffer[(SinglebandGeoTiff)]()	
		val lista_geotiffs_NIR = ListBuffer[(SinglebandGeoTiff)]()	
				
		for(a<-1 to 9)
		{
			println(a)
			val p1 = SinglebandGeoTiff(s"data/anomalias/landsat/DadosTratados/${a}B4A.tif")
			println(s"Lendo dado ${a}B4.TIF")
			
			lista_geotiffs_RED++= List(p1)
			val p2 = SinglebandGeoTiff(s"data/anomalias/landsat/DadosTratados/${a}B5A.tif")
			
			println(s"Lendo dado ${a}B5.TIF")
			lista_geotiffs_NIR++= List(p2)
		}
		
		val linhas = lista_geotiffs_RED(0).tile.rows
		val colunas = lista_geotiffs_RED(0).tile.cols
		var cellType = lista_geotiffs_RED(0).tile.cellType
		val ndvi = ArrayTile.empty(cellType,colunas,linhas).convert(DoubleCellType)
			
		
		def ndvi_calculo(nir: Double,red: Double): Double ={return ((nir-red)/(nir+red))}
		for(a<-1 to 8){	
			for(i<-0 to (colunas-1)){
				for(j<-0 to (linhas-1)){	
					val calculo = ndvi_calculo(lista_geotiffs_NIR(a).tile.get(i,j),lista_geotiffs_RED(a).tile.get(i,j))
					//println(calculo)
					ndvi.setDouble(i,j,calculo)				
				}
			}
			println(ndvi.tile.asciiDraw())
			val mb = ArrayMultibandTile(ndvi).convert(DoubleConstantNoDataCellType)
			println(s"Gerando mapa com NDVI ${a}...")
	    		MultibandGeoTiff(mb, lista_geotiffs_RED(0).extent, lista_geotiffs_RED(0).crs).write(s"data/saida/NDVI${a}.tif")
		}
	}
}
	
