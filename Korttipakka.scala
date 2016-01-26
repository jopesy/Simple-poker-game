import scala.util.Random
import scala.util.Sorting
import scala.math.Ordering._
import scala.collection.mutable.ArrayBuffer //ArrayBuffer, koska se on mutatoituva eli elementtejä voidaan lisätä ja poistaa, vastaa Javan ArrayListia (voidaan poistaa ja lisätät kortteja indeksin avulla)

/**Luokka, joka mallintaa 52:n kortin korttipakkaa (Ässä on numeroarvoltaan 14)*/
class Korttipakka {
  
  private val rnd: Random = new Random()
  private var pakka = new ArrayBuffer[Pelikortti]()
  private var ylinKortti = 0
  
  /**Alustaa pakan täyttämällä ArrayBufferin 52:lla kortilla*/
  def alustaPakka() ={
    for (i <- 1 to 4){
      for (j <- 2 to 14){
        val tempcard: Pelikortti = new Pelikortti(i, j)
        pakka += tempcard
      }
    }
    println("Pakka alustettu")
  }
  
  /**sekoittaa pakan*/
  def sekoita() = {
    pakka = rnd.shuffle(pakka)
    ylinKortti = 0
    println("Pakka sekoitettu")
  }
  
  /**palauttaa pakan päällimmäisen kortin ja poistaa kyseisen kortin pakasta*/
  def nostaKortti(): Pelikortti = {
    val nostettu = pakka(0)
    pakka.remove(0) // poistaa kortin pakasta, jottei samaa korttia voida nostaa uudestaan
    println("pakan koko: "+ pakka.size)
    return nostettu
  }
  def lisaaPakkaan(pelikortti: Pelikortti) ={
    pakka += pelikortti
  }
}