{
  lib,
  melpaBuild,
}:
melpaBuild {
  pname = "dv";
  version = "1.0.0";

  src = ./.;

  meta = {
    homepage = "https://github.com/theobori/dv";
    description = "Emacs dependencies visualizer";
    license = lib.licenses.gpl3Plus;
    maintainers = with lib.maintainers; [ theobori ];
  };
}
