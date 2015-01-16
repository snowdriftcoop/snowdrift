{ haskellPackages ? (import <nixpkgs> {}).haskellPackages }:

haskellPackages.cabal.mkDerivation (self: {
  pname = "Snowdrift";
  version = "0.0.2";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = with haskellPackages; [
    aeson async attoparsec authenticate blazeBuilder blazeHtml
    blazeMarkup clientsession cmdargs conduit dataDefault diagrams
    diagramsLib diagramsRasterific Diff emailValidate esqueleto
    fastLogger fileEmbed github hamlet hit hjsmin hourglass httpConduit
    httpTypes JuicyPixels liftedBase mime mimeMail monadControl
    monadLogger mtl mwcRandom pandoc pathPieces persistent
    persistentPostgresql persistentTemplate random regexTdfa resourcet
    semigroups shakespeare shakespeareCss shakespeareJs shakespeareText
    stm temporary text time transformers vector waiExtra waiLogger warp
    yaml yesod yesodAuth yesodAuthHashdb yesodCore yesodForm
    yesodMarkdown yesodNewsfeed yesodStatic
  ];
  testDepends = with haskellPackages; [
    esqueleto filepath haskellSrcExts haskellSrcMeta hspec htmlConduit
    httpTypes HUnit liftedBase monadControl monadLogger mtl network
    networkUri persistent persistentPostgresql resourcet semigroups
    text transformers waiExtra waiTest xmlConduit yesod yesodCore
    yesodMarkdown yesodRoutes yesodTest
  ];
  buildTools = with haskellPackages; [ cabalInstall ];
  meta = {
    homepage = "https://snowdrift.coop";
    description = "Infrastructure for Snowdrift fundrasing site";
    license = self.stdenv.lib.licenses.agpl3Plus;
    platforms = self.ghc.meta.platforms;
  };
})
