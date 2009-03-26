-- This plugin insert multimedia links in gitit wiki pages
{-

one can insert youtube and dailymotion videos, as well as single mp3 player or even an embedded mp3 player:

~~ {.youtube}
oHg5SJYRHA0
~~

~~ {.dailymotion}
x5thct
~~

~~ {.mp3}
http://www.labnol.org/assets/mp3/unofficial-yahoo-song.mp3
~~

~~ {.mp3player}
http://sd-12155.dedibox.fr/~charles/clara_clara_avec_agathe_-_mp3/playlist.xspf
~~

the flash mp3 players are 
* xspf web music player: http://musicplayer.sourceforge.net/
* google mp3 player
details of xspf playlist syntax can be found at:
http://www.xspf.org/quickstart/

license: GPL
written by pierre gueth <pierre.gueth@gmail.com>

-}

module MultimediaPlugin (plugin) where

import Gitit.Interface
import Network.URI (isURI)
import Data.Generics (everywhereM, mkM)
import Data.List (find)
import Data.Maybe (isJust,fromJust)

plugin :: Plugin
plugin = PageTransform transform

transform :: AppState -> Pandoc -> Web Pandoc
transform st = everywhereM (mkM (transformBlock st))

transformBlock :: AppState -> Block -> Web Block
transformBlock st (CodeBlock (id, classes, namevals) contents) | isJust match && validatef line && (not .null) line =
	return $ templatef line 
	where
		match = find (flip elem classes . plugin_classe) plugin_templates
		templatef = plugin_template $ fromJust match
		validatef = plugin_validate $ fromJust match
		line = (lines contents) !! 0 

transformBlock _ x = return x

data Template = Template {
	plugin_classe::String,
	plugin_template::(String -> Block),
	plugin_validate::(String -> Bool)} deriving Show

plugin_templates :: [Template]
plugin_templates = [
	Template "mp3" mp3Template isURI,
	Template "mp3player" mp3playerTemplate isURI,
	Template "dailymotion" dailymotionTemplate isId,
	Template "youtube" youtubeTemplate isId]

mp3Template :: String -> Block
mp3Template id = RawHtml $
	"<div class='multimedia'>" ++
	"<embed type='application/x-shockwave-flash' src='http://www.google.com/reader/ui/3247397568-audio-player.swf?audioUrl=" ++ id ++
	"' width='400' height='27' allowscriptaccess='never' quality='best' bgcolor='#ffffff' wmode='window' flashvars='playerMode=embedded' />" ++
	"</div>"
	
youtubeTemplate :: String -> Block
youtubeTemplate id = RawHtml $ 
	"<div class='multimedia'>" ++
	"<embed src='" ++ url ++ "' type='application/x-shockwave-flash' allowscriptaccess='never' allowfullscreen='true' width='445' height='364' />" ++
	"</div>"
	where url="http://www.youtube.com/v/" ++ id

dailymotionTemplate :: String -> Block
dailymotionTemplate id = RawHtml $
	"<div>" ++
	"<embed src='" ++ url ++ "' type='application/x-shockwave-flash' width='420' height='339' allowFullScreen='true' allowScriptAccess='never' />" ++
	"</div>"
	where url="http://www.dailymotion.com/swf/" ++ id

mp3playerTemplate :: String -> Block
mp3playerTemplate id = RawHtml $
	"<div>" ++
	"<object type='application/x-shockwave-flash' width='400' height='170' data='" ++ url ++ "' >" ++
	"<param name='movie' value='" ++ url ++ "' />" ++
	"</object>" ++
	"</div>"
	where url="http://sd-12155.dedibox.fr/xspf_player.swf?playlist_url=" ++ id

isId :: String -> Bool
isId = all (\x ->  any (elem x) [['a'..'z'],['0'..'9'],['A'..'Z']]) 

