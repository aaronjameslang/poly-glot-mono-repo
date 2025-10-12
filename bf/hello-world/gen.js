const { write } = require("fs");

const writer = require("fs").createWriteStream(__dirname + "/hello-world.bf");

"Hello World!\n".split("").forEach((c) => {
  const code = c.charCodeAt(0);
  const x = Array.from({ length: code }).fill("+").join("");
  writer.write(JSON.stringify(c) + " = " + code + "\n");
  writer.write(x + "\n");
  writer.write(". out > next\n");
});
