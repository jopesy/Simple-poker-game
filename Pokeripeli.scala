import scala.collection.mutable.ArrayBuffer

/**Luokka, joka mallintaa pokeripeliä*/
class Pokeripeli {
  var pelaajat = new ArrayBuffer[Pelaaja]()
  private var pakka = new Korttipakka()
  
  /**Lisää pelaajan peliin*/
  def lisaaPelaaja(pelaaja: Pelaaja) ={
    pelaajat += pelaaja
  }
  /**Poistaa pelaajan pelistä*/
  def poistaPelaaja(pelaaja: Pelaaja) ={
    pelaajat -= pelaaja
  }
  def jaaKortit() ={
    pakka.alustaPakka()
    pakka.sekoita()
    for(i <- 1 to 5){ //5 korttia jokaiselle pelaajalle
      for(j <- 0 to pelaajat.size-1){
       pelaajat(j).otaKortti(pakka.nostaKortti()) 
      }      
    }
    for(i <- 0 to pelaajat.size-1){
      pelaajat(i).annaKasi.jarjestaKasi() //järjestää jokaisen pelaajan kortit järjestykseen
    }
  }
  def vaihdaKortti(pelaaja: Pelaaja, indeksi: Int) ={
    pelaaja.poistaKortti(indeksi)
    pelaaja.otaKortti(pakka.nostaKortti()) //Pelaaja lisää nostamansa kortin käteensä
  }
  /**Palauttaa pelaajan, jolla on paras käsi
   * @return voittaja
   */
  def annaVoittaja(): Pelaaja ={
    var voittaja: Pelaaja = pelaajat(0)
    var voittokasi: Pokerikasi = voittaja.annaKasi()
    var temp = 0
    for(temp <- pelaajat){
      if(temp.annaKasi() > voittaja.annaKasi()){
        voittaja = temp
        voittokasi = temp.annaKasi()
      }
    }
    return voittaja
  }
}