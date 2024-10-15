data TipoPlanta = PlenoSol | MediaSombra | Floral | Frutal

data TipoSemilla = Hortalizas | Aromaticas | Cesped

data TipoMaceta = Terracota | FibroCemento | Plastico

data Ubicacion = Exterior | Interior

data Agua = MuyResistente | Resistente | NadaResistente

data Forma = Cuadrada | Redonda

type Peso = Float

type Precio = Int

-- a)
data ProductoDeVivero
  = Planta TipoPlanta Ubicacion Agua Precio
  | BolsaSemilla TipoSemilla Peso Precio
  | Maceta TipoMaceta Forma Precio

-- b)
cuantasPlantas :: [ProductoDeVivero] -> TipoPlanta -> Int
cuantasPlantas [] _ = 0
cuantasPlantas (Planta tp _ _ _ : pvs) tp' | eqTipoPlanta tp tp' = 1 + cuantasPlantas pvs tp'
cuantasPlantas pvs tp' = cuantasPlantas pvs tp'

-- c)

instance Eq ProductoDeVivero where
  (Planta t u _ _) == (Planta t' u' _ _) = eqTipoPlanta t t' && eqUbicacion u u'
  (BolsaSemilla _ p pe) == (BolsaSemilla _ p' pe') = p == p' && pe == pe'
  (Maceta t _ _) == (Maceta t' _ _) = eqTipoMaceta t t'
  _ == _ = False

eqTipoMaceta :: TipoMaceta -> TipoMaceta -> Bool
eqTipoMaceta Terracota Terracota = True
eqTipoMaceta FibroCemento FibroCemento = True
eqTipoMaceta Plastico Plastico = True
eqTipoMaceta _ _ = False

eqTipoPlanta :: TipoPlanta -> TipoPlanta -> Bool
eqTipoPlanta PlenoSol PlenoSol = True
eqTipoPlanta MediaSombra MediaSombra = True
eqTipoPlanta Floral Floral = True
eqTipoPlanta Frutal Frutal = True
eqTipoPlanta _ _ = False

eqUbicacion :: Ubicacion -> Ubicacion -> Bool
eqUbicacion Exterior Exterior = True
eqUbicacion Interior Interior = True
eqUbicacion _ _ = False