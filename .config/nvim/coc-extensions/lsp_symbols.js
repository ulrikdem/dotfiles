const {events, languages, workspace} = require('coc.nvim');

const timers = {};
const tokens = {};

exports.activate = ({subscriptions}) => {
    subscriptions.push(
        workspace.onDidOpenTextDocument(doc => update(getBufnr(doc))),

        workspace.onDidCloseTextDocument(doc => {
            const bufnr = getBufnr(doc);
            cancel(bufnr);
            setSymbols(bufnr, []);
        }),
    );

    for (const event of ['TextChanged', 'InsertLeave'])
        events.on(event, update, null, subscriptions);
    events.on('InsertEnter', cancel, null, subscriptions);
};

function getBufnr({uri}) {
    return workspace.getDocument(uri).bufnr;
}

function update(bufnr) {
    cancel(bufnr);

    timers[bufnr] = setTimeout(async () => {
        delete timers[bufnr];

        const doc = workspace.getDocument(bufnr);
        if (!doc)
            return;

        const symbols = await languages.documentSymbolManager.provideDocumentSymbols(
            doc.textDocument,
            tokens[bufnr] = languages.token,
        );
        delete tokens[bufnr];

        if (symbols)
            setSymbols(bufnr, symbols);
    }, 250);
}

function setSymbols(bufnr, symbols) {
    workspace.nvim.lua('lsp_symbols.update(...)', [bufnr, symbols]);
}

function cancel(bufnr) {
    if (bufnr in timers) {
        clearTimeout(timers[bufnr]);
        delete timers[bufnr];
    }

    if (bufnr in tokens) {
        tokens[bufnr].cancel();
        delete tokens[bufnr];
    }
}
