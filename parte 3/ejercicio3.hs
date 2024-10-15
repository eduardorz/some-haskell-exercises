-- a)
type Altura = Int

type TotalHojas = Int

type HojasFlorecidas = Int

type Meses = Int

data EstadoHojas = MuchasQuemadas | PocasQuemadas | Saludables deriving (Show, Eq)

data RegistroPlanta = NoDato | DatoPlanta EstadoHojas Altura TotalHojas HojasFlorecidas Meses RegistroPlanta

-- b)
trasplantar :: RegistroPlanta -> Int -> Bool
trasplantar (DatoPlanta Saludables a _ _ m _) m' | 40 <= a && m' <= m = True
trasplantar (DatoPlanta PocasQuemadas a _ _ m _) m' | 40 <= a && m' <= m = True
trasplantar (DatoPlanta MuchasQuemadas _ th _ m _) m' | th <= 10 && m' <= m = True
trasplantar (DatoPlanta _ _ _ _ _ rp) m' = trasplantar rp m'
trasplantar _ _ = False

-- c)
devolverEstado :: RegistroPlanta -> Int -> Maybe EstadoHojas
devolverEstado (DatoPlanta eh _ _ _ m rp) m'
  | m' <= m = Just eh
  | otherwise = devolverEstado rp m'
devolverEstado _ _ = Nothing