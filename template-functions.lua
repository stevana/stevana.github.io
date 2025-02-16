function Meta(meta)
    if meta.title then
        meta.escaped_title = pandoc.utils.stringify(meta.title):gsub(" ", "+")
    end
    return meta
end
