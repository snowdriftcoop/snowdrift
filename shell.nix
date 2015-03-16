{ pkgs ? (import <nixpkgs> {})
, haskellPackages ? pkgs.haskellPackages
}:

haskellPackages.cabal.mkDerivation (self: {
  pname = "Snowdrift";
  version = "0.0.2";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = with haskellPackages; [
    async attoparsec authenticate blazeBuilder blazeHtml blazeMarkup
    cmdargs conduit dataDefault Diff emailValidate esqueleto fastLogger
    github hit hjsmin hourglass httpConduit httpTypes liftedBase mime
    mimeMail monadLogger mtl mwcRandom pandoc pathPieces persistent
    persistentPostgresql persistentTemplate random regexTdfa resourcet
    semigroups shakespeare stm temporary text time titlecase transformers
    waiExtra waiLogger yaml yesod yesodAuth yesodAuthHashdb yesodCore
    yesodForm yesodMarkdown yesodNewsfeed yesodStatic
  ];
  testDepends = with haskellPackages; [
    esqueleto filepath haskellSrcExts haskellSrcMeta hspec htmlConduit
    httpTypes HUnit liftedBase monadControl monadLogger networkUri
    persistent semigroups text transformers waiExtra xmlConduit yesod
    yesodMarkdown yesodTest
  ];
  buildTools =
    (with pkgs; [ git ]) ++
    (with haskellPackages; [ cabalInstall ]);
  meta = {
    homepage = "https://snowdrift.coop";
    description = "Infrastructure for Snowdrift fundrasing site";
    license = self.stdenv.lib.licenses.agpl3Plus;
    platforms = self.ghc.meta.platforms;
  };
})
