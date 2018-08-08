// zouv
// 2017
// proto 协议打包、解析，路由

var ProtoManager = {
    pbRoot: null,
    protoHandle: null,
    Init: function(protoHandle) {
        var self = this;
        protobuf.load("proto/p01_login.proto", function(err, root) {
            console.log("root : ", root);
            if (err)
                console.log("register proto file Error!");
            self.pbRoot = root;
        });
        self.protoHandle = protoHandle;
    },
    Encode: function(protoName, msg) {
        console.log("protoName1 : ", protoName, msg);
        var handler = this.pbRoot.lookup(protoName);
        var data = handler.create(msg);
        var buffer = handler.encode(data).finish();

        // 把协议id放进去
        var protoId = this.GetProtoId(protoName);

        var arrProtoId = UtilNet.Int16ToBytes(protoId);

        var arr = UtilNet.ContactBytes(arrProtoId, buffer);


        return arr;
    },
    Decode: function(protoName, buffer) {
        var handler = this.pbRoot.lookup(protoName);
        var msg = handler.decode(buffer);　 // TODO 去掉前2字节
        return msg;
    }
}

ProtoManager.GetProtoId = function(protoName) {
    switch (protoName) {
        case "protopackage.cs_login":
            return 101;
        case "protopackage.cs_move":
            return 102;
        default:
            console.log("!!! get proto id Warning! not find : ", protoName);
            return 0;
    }
};

ProtoManager.GetProtoName = function(protoId) {
    switch (protoId) {
        case 102:
            return "protopackage.sc_login";
        default:
            console.log("!!! get proto name Warning! not find : ", protoId);
            return "";
    }
};

ProtoManager.DispatchMsg = function(protoName, msg) {
    switch (protoName) {
        case "protopackage.sc_login":
            this.protoHandle.sc_login(msg);
            break;
        default:
            console.log("!!! dispatch msg Warning! not find proto func: ", protoName);
    }
}