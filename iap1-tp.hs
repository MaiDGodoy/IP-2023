

----- Completar con los datos del grupo

-- Nombre de Grupo: xx
-- Integrante 1: Nombre Apellido, email, LU
-- Integrante 2: Nombre Apellido, email, LU
-- Integrante 3: Nombre Apellido, email, LU
-- Integrante 4: Nombre Apellido, email, LU

type Usuario = (Integer, String) -- (id, nombre)
type Relacion = (Usuario, Usuario) -- usuarios que se relacionan
type Publicacion = (Usuario, String, [Usuario]) -- (usuario que publica, texto publicacion, likes)
type RedSocial = ([Usuario], [Relacion], [Publicacion])



-- Funciones basicas


usuarios :: RedSocial -> [Usuario]
usuarios (us, _, _) = us

relaciones :: RedSocial -> [Relacion]
relaciones (_, rs, _) = rs

publicaciones :: RedSocial -> [Publicacion]
publicaciones (_, _, ps) = ps

idDeUsuario :: Usuario -> Integer
idDeUsuario (id, _) = id 

nombreDeUsuario :: Usuario -> String
nombreDeUsuario (_, nombre) = nombre 

usuarioDePublicacion :: Publicacion -> Usuario
usuarioDePublicacion (u, _, _) = u

likesDePublicacion :: Publicacion -> [Usuario]
likesDePublicacion (_, _, us) = us

-- Ejercicios

nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios = undefined

-- describir qué hace la función: .....
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe = undefined

-- describir qué hace la función: .....
cantidadDeAmigos :: RedSocial -> Usuario -> Integer
cantidadDeAmigos = undefined

-- describir qué hace la función: .....
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos = undefined

-- describir qué hace la función: .....
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos = undefined

-- describir qué hace la función: .....
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe = undefined

-- describir qué hace la función: .....
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA = undefined

-- describir qué hace la función: .....
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones = undefined

-- describir qué hace la función: .....
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel = undefined

-- describir qué hace la función: .....
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos = undefined


--auxiliares

pertenece :: (Eq a) => a ->[a] -> Bool
pertenece e[]= False
pertenece e(x:xs) = e == x || pertenece e xs

mismosElementos :: (Eq a) => [a]-> [a]-> Bool
mismosElementos [][]=True
mismosElementos _[]=False
mismosElementos e(x:xs)=pertenece x e  && (mismosElementos xs) (quitar x e)
    where quitar _[]=[]
          quitar e(y:ys)
                |e == y = quitar e ys
                |otherwise = y:quitar e ys
 
sinRepetidos :: (Eq t) => [t] -> Bool
sinRepetidos [] = True
sinRepetidos (x:xs)
  | pertenece x xs = False
  | otherwise = sinRepetidos xs

empiezaCon :: Eq a => a -> [a] -> Bool
empiezaCon _ [] = False
empiezaCon e (x:_) = e == x

terminaCon :: Eq a => a -> [a] -> Bool
terminaCon _ [] = False
terminaCon e [x] = e == x                --verifica si la lista tiene solo un elemento, en cuyo caso verifica si ese elemento es igual al elemento buscado
terminaCon e (x:xs) = terminaCon e xs    --Si la lista tiene más de un elemento, la función se llama a sí misma con la cola de la lista  como segundo argumento.

relacionadosDirecto :: Usuario -> Usuario -> RedSocial -> Bool
relacionadosDirecto u1 u2 red = (u1,u2) `pertenece` relaciones red || (u2,u1) `pertenece` relaciones red --verifica si los usuarios(u1,u2) están relacionados directamente en la red social 'red'
                                                                                                         --se utiliza la función 'pertenece' para verificar esa relación


sonDeLaRed :: RedSocial -> [Usuario] -> Bool
sonDeLaRed red [] = True                                                 --caso base: lista vacia
sonDeLaRed red (u:us) = pertenece u (usuarios red) && sonDeLaRed red us  --se toma a el 1er usuario (u) de la lista para verificar q pertenezca
                                                                         --si 'u' pertenece,se llama recursivamente a la función sonDeLaRed con el resto de la lista 'us'

noHayPublicacionesRepetidas :: [Publicacion] -> Bool
noHayPublicacionesRepetidas [] = True                             -- Caso base: lista vacía
noHayPublicacionesRepetidas [_] = True                            -- Caso base: una publicación en la lista
noHayPublicacionesRepetidas ((u1, _, _):(u2, _, _):ps)            -- Caso recursivo: dos publicaciones en la lista
    | idDeUsuario u1 == idDeUsuario u2 = False                    -- Si los usuarios son iguales, hay una publicación repetida
    | otherwise = noHayPublicacionesRepetidas ((u1,"", []) : ps) -- Sigue verificando la lista sin la segunda publicación

cadenaDeAmigos :: [Usuario] -> RedSocial -> Bool
cadenaDeAmigos [] _ = True
cadenaDeAmigos [_] _ = True
cadenaDeAmigos (u1:u2:us) red = relacionadosDirecto u1 u2 red && cadenaDeAmigos (u2:us) red

usuariosLikeValidos :: [Usuario] -> [Usuario] -> Bool
usuariosLikeValidos _ [] = True
usuariosLikeValidos [] _ = False
usuariosLikeValidos usl (u:us) = pertenece u usl && usuariosLikeValidos usl us

--verifica si todas las relaciones en una lista son asimétricas
--si (u1, u2) es una relación, entonces (u2, u1) no es una relación en la misma secuencia
relacionesAsimetricas :: [Relacion] -> Bool
relacionesAsimetricas [] = True
relacionesAsimetricas ((u1,u2):rs) = not (pertenece (u2,u1) rs) && relacionesAsimetricas rs

noHayRelacionesRepetidas :: [Relacion] -> Bool
noHayRelacionesRepetidas [] = True
noHayRelacionesRepetidas ((u1,u2):rs) = noHayRelacionesRepetidasAux (u1,u2) rs && noHayRelacionesRepetidas rs
  where noHayRelacionesRepetidasAux _ [] = True
        noHayRelacionesRepetidasAux (u1,u2) ((u1',u2'):rs) = 
          (idDeUsuario u1 /= idDeUsuario u1' || idDeUsuario u2 /= idDeUsuario u2') &&
          (idDeUsuario u1 /= idDeUsuario u2' || idDeUsuario u2 /= idDeUsuario u1') &&
          noHayRelacionesRepetidasAux (u1,u2) rs

longitud :: [a] -> Integer
longitud [] = 0
longitud (_:xs) = 1 + longitud xs                             --cuenta los elementos de la lista recursivamente

usuarioValido :: Usuario -> Bool
usuarioValido (id, nombre) = id > 0 && longitud nombre > 0   -- verificar que el id del usuario sea mayor a cero y que la longitud del nombre sea mayor a cero también.  

usuariosNoRepetidos :: RedSocial -> Bool
usuariosNoRepetidos (us, _, _) = noHayIdsRepetidos us

noHayIdsRepetidos :: [Usuario] -> Bool
noHayIdsRepetidos [] = True
noHayIdsRepetidos (u:us) = idDeUsuarioNoEstaEnLista u us && noHayIdsRepetidos us

idDeUsuarioNoEstaEnLista :: Usuario -> [Usuario] -> Bool
idDeUsuarioNoEstaEnLista _ [] = True
idDeUsuarioNoEstaEnLista u (u':us') = idDeUsuario u /= idDeUsuario u' && idDeUsuarioNoEstaEnLista u us'
--idDeUsuarioNoEstaEnLista u (u':us') = not (idDeUsuario u == idDeUsuario u') && idDeUsuarioNoEstaEnLista u us'

usuariosValidos :: [Usuario] -> Bool
usuariosValidos [] = True
usuariosValidos (u:us) = usuarioValido u && noHayIdsRepetidos (u:us) && usuariosValidos us

likesDePublicacionSonUsuariosDeRed :: [Usuario] -> [Usuario] -> Bool
likesDePublicacionSonUsuariosDeRed [] _ = True
likesDePublicacionSonUsuariosDeRed (like:likes) usuariosDeRed = perteneceUsuario like usuariosDeRed && likesDePublicacionSonUsuariosDeRed likes usuariosDeRed

perteneceUsuario :: Usuario -> [Usuario] -> Bool
perteneceUsuario _ [] = False
perteneceUsuario u (u':us') = idDeUsuario u == idDeUsuario u' || perteneceUsuario u us'

usuariosDeRelacionValidos :: [Usuario] -> [Relacion] -> Bool
usuariosDeRelacionValidos _ [] = True
usuariosDeRelacionValidos [] _ = False
usuariosDeRelacionValidos us ((u1,u2):rs) =  pertenece u1 us && pertenece u2 us && u1 /= u2 && usuariosDeRelacionValidos us rs

relacionesValidas :: [Usuario] -> [Relacion] -> Bool
relacionesValidas us rels = usuariosDeRelacionValidos us rels && relacionesAsimetricas rels && noHayRelacionesRepetidas rels

usuariosDePublicacionSonUsuariosDeRed :: [Usuario] -> [Publicacion] -> Bool
usuariosDePublicacionSonUsuariosDeRed _ [] = True
usuariosDePublicacionSonUsuariosDeRed us (p:ps) =
  pertenece (usuarioDePublicacion p) us && usuariosDePublicacionSonUsuariosDeRed us ps

publicacionesValidas :: [Usuario] -> [Publicacion] -> Bool
publicacionesValidas us pubs = usuariosDePublicacionSonUsuariosDeRed us pubs && likesDePublicacionSonUsuariosDeRed us pubs && noHayPublicacionesRepetidas pubs

redSocialValida :: RedSocial -> Bool
redSocialValida red = usuariosValidos us &&
                      relacionesValidas us rs &&
                      publicacionesValidas us ps                               