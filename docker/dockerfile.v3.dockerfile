FROM ghcr.io/nathansam/predicct:1.0.1

# Add PDF render support
RUN wget -qO- "https://yihui.org/tinytex/install-bin-unix.sh" | sh

RUN /root/bin/tlmgr update --self
RUN /root/bin/tlmgr update --all --no-persistent-downloads

RUN /root/bin/tlmgr install  --no-persistent-downloads koma-script caption multirow wrapfig colortbl pdflscape tabu threeparttable threeparttablex environ ulem makecell

RUN  /root/bin/tlmgr conf tlmgr persistent-downloads 0

COPY ./docker/render render
RUN chmod u+x render
CMD ["./render"]
