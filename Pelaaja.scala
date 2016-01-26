
class Pelaaja(n: String) {
  var nimi = n
  var kasi: Pokerikasi = new Pokerikasi()
  
  def otaKortti(kortti: Pelikortti) ={
    kasi.lisaaKateen(kortti)
  }
  def poistaKortti(indeksi: Int) ={
    kasi.poistaKadesta(indeksi)
  }
  def annaKasi(): Pokerikasi ={
    return kasi
  }
  override def toString(): String ={
    nimi
  }
}