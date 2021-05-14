# MappingConverter

*MappingConverter* is a tool to convert and transform different minecraft mappings in different formats.

In *MappingConverter* a mapping is something that can map values. There are three different names from/to where something can be mapped. These are:

  * `obfuscated`/`obf` which normally represents the obfuscated names found in the jar files by mojang.
  * `srg`/`intermediary` normally represents mappings that are unique and stay the same between versions.
  * `mapped`/`named` normally represents human readable names.

As mappings have different information, *MappingConverter* needs a way to represent the mappings internally. Here's an overview:

  * Mappings can have field types, but they don't have to, depending on the format. *MappingConverter* will try to put field type information to all mappings that it can when performing its operation. Trying to write mappings without field type information to a format which requires field types will fail.

  * Some mappings give constructors names to reference them in other mappings. That's why *MappingConverter* supports pseudo names for constructors.

  * MCP mappings are just unique mappings from one value to another. However, there are no class mappings and no method signatures to these need to be represented differently than other mappings. So a mapping can also have these unique mappings. They'll be converted to normal mappings as soon as possible.

  * There are formats that just map constructor signatures to names/ids that are used by other mappings. For these constructor mappings exist, that have no class mappings. They are pretty useless on their own but can be applied to other mappings which have class information to be used.

  * Some mappings have side information, others don't. However, you can write mappings without side information to a format which has side information. In that case, everything is just written as if it existed on both sides.

*MappingConverter* supports the following formats:

  * `SRG`/`CSRG`/`TSRG`: Mappings from obfuscated names to srg names used by MCP
  * `CTOR`: Can be used to read the `constructors.txt` file used by MCP.
  * `EXC`: Used by MCP in older versions for exceptions, access information and constructor names. *MappingConverter* will ignore everything, but the constructor names from these files.
  * `MCP`: Used to read and write ZIP files containing `fields.csv`, `methods.csv` and optionally `params.csv` with unique mappings from SRG to Mapped
  * `PRO`: Proguard mappings from Mapped to Obfuscated. Can be used to read the official mappings. Writing ProGuard mappings will insert dummy line numbers for methods.
  * `TINY`/`TINYv2`: Fabrics mappings. Can map from Obfuscated to Intermediary or from Obfuscated to Intermediary and Named.
  * `MCPC`: (MCP config) Reads a ZIP file and looks for `srg`/`csrg`/`tsrg`/`exc` and `constructor.txt` files in it or in a folder named `config`. This will have side information if the SRG is split and will already have constructor information applied if possible. You can also write this in which case any possible file which is looked for while reading is put into the ZIP inside the folder named `config`.

The following table shows which names must be present on a mapping in order to use it with a format:

| Format | Names |
| :--- | :--- |
| `SRG`/`CSRG`/`TSRG` | `obfuscated`/`obf` and `srg`/`intermediary`  |
| `CTOR` | `srg`/`intermediary` |
| `EXC` | `srg`/`intermediary` |
| `MCP` | `srg`/`intermediary` and `mapped`/`named` |
| `PRO` | `obfuscated`/`obf` and `mapped`/`named` |
| `TINY`/`TINYv2` | `obfuscated`/`obf`, `srg`/`intermediary` and optionally `mapped`/`named` |
| `MCPC` | `obfuscated`/`obf` and `srg`/`intermediary` |

Running the program without arguments will get you an interactive input where you can execute one statement after another. To end the program, press `Ctrl`+`D` or type `exit`

You can also run the program with a file argument to a file that specifies the statements to execute. Inside that file you can have line comments starting with a `#` (only at line start). The recommended file suffix is `mcm`. After the argument that contains the file name, you can put as many arguments of type `name=value`. For each of them `${name}` will get replaced by the value in the file.

There's no need to put things in quotes except for path names that contain characters not in `A-Za-z0-9/\.$`.

A statement is either a goal, or an assignment which looks like `name = goal`. Then you can use the result of that goal later on with the given name. Also, the result of the first statement will be accessible ith `%0`, the result of the second statement with `%1` and so on.

A goal is something that takes some parameters and produces a mapping. Here's a table of all goals:

| goal | arguments | description |
| :--- | :--- | :--- |
| `write(goal, format, path)` | `goal`: The mappings to use. <br> `format`: The format to use (lowercase) <br> `path`: Output file path | Writes mappings to a file using a given format. The result of this are the mappings written. |
| `read(format, path)` | `format`: The format to use (lowercase) <br> `path`: Input file path | Read mappings from a file using a given format. |
| `mc(version)` | `version`: A minecraft version | Loads the official mappings for a minecraft version and merges them, so it has side information available. |
| `srg(version)` | `version`: A minecraft version | Loads SRG mappings for a minecraft version and apply constructor information to it. (Loads the mcp config ZIP with `MCPC`). This also works for versions prior to 1.12. |
| `intermediary(version)` | `version`: A minecraft version | Loads fabric intermediary mappings for a minecraft version. |
| `mcp(version)` | `version`: A MCP mappings version | Loads MCP mappings for a given version. |
| `yarn(version)` | `version`: A Yarn mappings version | Loads Yarn mappings for a given version. |
| `merge(names, goals...)` | `names`: The names used for merging. <br> `goals`: A list of mappings to merge | Merges multiple mappings together. The given names state which names are used to find duplicates. For example if you want to merge intermediary and srg, you'd need to merge toward `obfuscated` as it's the only common names. If something is found in one of the mappings, the subsequent mappings are no longer searched with one exception: If subsequent mappings have a match that have other side information, side information is merged as well. This means you can merge for example a client and a server SRG to get side information. The result will only have the intersection of names of the names present in the mappings that are merged. |
| `safe_merge(names, safe, goal1, goal2)` or `safe(names, safe, goal1, goal2)` | `names`: The names used for merging. <br> `safe`: The names used for checking duplicates. <br> `goal1`/`goal2`: The two mappings to be merged. | Merges two mappings together but skips everything from the second mappings than can be mapped by the first mappings through the names given by `safe`. |
| `transform(goal, transformation...)` | `goal`: The mappings to transform <br> `transformations`: A list of transformations to apply. These are in format `from -> to` where from and to are names. | Transforms mappings by placing contents that were mapped to specific names to be mapped to other names. For example having some mappings with obfuscated and srg names and transforming them with `obf -> srg, srg -> obf` will reverse the mappings. When writing them as SRG you'll get a reverse SRG. |
| `reverse(goal)` or `rev(goal)` | `goal`: The mappings to reverse | Same as transforming with `obf -> srg, srg -> obf` |
| `mapping_reverse(goal)` or `mreverse(goal)` or `mrev(goal)` | `goal`: The mappings to reverse | Same as transforming with `srg -> mapped, mapped -> srg` |
| `apply(srg, mapped)` | `srg`: Mappings from obfuscated to srg <br> `mapped`: Mappings from srg to named | Applies mappings together based on srg names. For example applying srg mappings and mcp mappings together will get you new mappings with contain all three names. If something is mapped by the first mappings but not by the second mappings, it's mapped name will be the same as the name in srg/intermediary.  |
| `partial_apply(srg, mapped)` or `partial(srg, mapped)` | `srg`: Mappings from obfuscated to srg <br> `mapped`: Mappings from srg to named | Same as `apply(srg, mapped)` but things that are not mapped by the second mappings won't get the srg/intermediary name but will be completely dropped from the resulting mappings. |
| `obf_apply(srg, mapped)` or `obfmap(srg, mapped)` | `srg`: Mappings from obfuscated to srg <br> `mapped`: Mappings from obfuscated to named | Same as `apply(srg, mapped)` but this time not the srg/intermediary names will be considered the common names that are used to apply the mappings but the obfuscated names. This is useful for applying thing to the official mappings. |
| `sided(goal, side)` | `goal`: The mappings to modify side information <br> `side`: Target side information | Gets new mappings where each element with unknown side information gets the given side information. (side must be one of `client`, `server` or `both` |
| `force_sided(goal, side)` or `force(goal, side)` | `goal`: The mappings to modify side information <br> `side`: Target side information | Same as `sided(goal, side)` but elements that already have side information will get replaced their side information with the given. |
| `filter_sided(goal, client, server)` or `filter(goal, client, server)` | `goal`: The mappings to filter <br> `client`: Whether the filtered mappings must be present on the client <br> `server`: Whether the filtered mappings must be present on the server | Filters everything out from the given mappings that is not present on the client/server. |
| `contructor_apply(names, goal, ctors)`, `ctor_apply(names, goal, ctors)` or `ctor(names, goal, ctors)` | `names`: The names in which the constructor information is available <br> `goal`: The mappings that should get constructor information applied <br> `ctors`: The mappings that contain constructor information | Removes all constructor information from the first mappings and replaces it with the constructor information from the second mappings. `names` are the common names of these mapping. (the names in which they have the same values) |
| `field_types_apply(names, goal, ctors)`, `ftypes_apply(names, goal, ctors)` or `ftypes(names, goal, ctors)` | `names`: The names in which the field type information is available <br> `goal`: The mappings that should get field type information applied <br> `ctors`: The mappings that contain field type information | Same as `ctor(names, goal, ctors)` but for field types. |

Here's an example to build MCP mappings that map everything like `snapshot_20210309-1.16.5` and takes everything not mapped from the official mappings:

```
official = mc(1.16.5)
srg = srg(1.16.5)
srg_offcial = obfmap(srg, official)
mapped = mcp(snapshot_20210309-1.16.5)
srg_mapped = partial(srg_offcial, mapped)
write(merge(srg, srg_mapped, srg_offcial), mcp, mappings.zip)
```