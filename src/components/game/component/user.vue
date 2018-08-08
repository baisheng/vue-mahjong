<template>
  <div class="user">

    <!--下家-->
    <!--{{sites.site2.PlayerId == ownerId ? '庄家' : ''}}-->
    <div class="item u2" flex="dir:top main:center cross:center">
      <div flex="dir:top main:center cross:center" v-show="sites.site2.position">
        <span>{{sites.site2.playerName}} <b class="banker"><img v-if="sites.site2.PlayerId == ownerId"
                                                                src="../../../assets/nav_icon/banker.png"
                                                                alt=""></b></span>
        <b class="point">{{ sites.site2.PlayerId | change(record)}}</b>
      </div>
      <div class="flower" v-show="sites.site2.position">
        <strong v-if="showFlower[1].isZ"><img src="../../../assets/nav_icon/flower1.png" alt="">
          x{{sites.site2.PlayerId | zhenghua('2', all_users)}}</strong>
        <strong v-if="showFlower[1].isY"><img src="../../../assets/nav_icon/flower2.png" alt="">
          x{{sites.site2.PlayerId | yehua('2', all_users)}}</strong><br>

      </div>

      <p v-show="!sites.site2.position" @click="doSeat(2)" flex="main:center cross:center">坐下</p>

    </div>
    <!--对家-->
    <div class="item u4" flex="dir:top main:center cross:center">
      <div flex="dir:top main:center cross:center" v-show="sites.site3.position">
        <span>{{sites.site3.playerName}}<b class="banker"><img v-if="sites.site3.PlayerId == ownerId"
                                                               src="../../../assets/nav_icon/banker.png"
                                                               alt=""></b></span>
        <span v-show="isR.p2">已准备</span>
        <b class="point">{{ sites.site3.PlayerId | change(record)}}</b>

      </div>
      <div class="flower" v-show="sites.site3.position">
        <strong v-if="showFlower[2].isZ"><img src="../../../assets/nav_icon/flower1.png" alt="">
          x{{sites.site3.PlayerId | zhenghua('3', all_users)}}</strong>
        <strong v-if="showFlower[2].isY"><img src="../../../assets/nav_icon/flower2.png" alt="">
          x{{sites.site3.PlayerId | yehua('3', all_users)}}</strong><br>

      </div>

      <p v-show="!sites.site3.position" @click="doSeat(3)" flex="main:center cross:center">坐下</p>


    </div>
    <!--上家-->
    <div class="item u1" flex="dir:top main:center cross:center">
      <div flex="dir:top main:center cross:center" v-show="sites.site4.position">
        <span>{{sites.site4.playerName}}<b class="banker"><img v-if="sites.site4.PlayerId == ownerId"
                                                               src="../../../assets/nav_icon/banker.png"
                                                               alt=""></b></span>
        <span v-show="isR.p3">已准备</span>
        <b class="point"> {{sites.site4.PlayerId | change(record)}}</b>
      </div>
      <div class="flower" v-show="sites.site4.position">
        <strong v-if="showFlower[3].isZ"><img src="../../../assets/nav_icon/flower1.png" alt="">
          x{{sites.site4.PlayerId | zhenghua('4', all_users)}}</strong>
        <strong v-if="showFlower[3].isY"><img src="../../../assets/nav_icon/flower2.png" alt="">
          x{{sites.site4.PlayerId | yehua('4', all_users)}}</strong><br>

      </div>

      <p v-show="!sites.site4.position" @click="doSeat(4)" flex="main:center cross:center">坐下</p>

    </div>
    <!--自己-->
    <div class="item u3" flex="dir:top main:center cross:center">
      <!--玩家信息-->
      <div flex="dir:top main:center cross:center" v-show="sites.site1.position">
        <span>{{sites.site1.playerName}}<b class="banker"><img v-if="sites.site1.PlayerId == ownerId"
                                                               src="../../../assets/nav_icon/banker.png"
                                                               alt=""></b></span>
        <span v-show="isR.p4">已准备</span>
        <b class="point">{{ sites.site1.PlayerId | change(record)}}</b>
      </div>

      <!--正野花-->
      <div class="flower" v-show="sites.site1.position">
        <strong v-if="showFlower[0].isZ"> <img src="../../../assets/nav_icon/flower1.png" alt="">
          x{{sites.site1.PlayerId | zhenghua('1', all_users)}}</strong>
        <strong v-if="showFlower[0].isY"><img src="../../../assets/nav_icon/flower2.png" alt="">
          x{{sites.site1.PlayerId | yehua('1', all_users)}}</strong><br>

      </div>
      <!--坐下状态-->
      <p v-show="!sites.site1.position" @click="doSeat(1)" flex="main:center cross:center">坐下</p>

    </div>


    <!--掉线-->
    <div class="reconnect_time rt1" flex="main:center cross:center" v-show="reConnect[0].current">
      {{reConnect[0].current}}
    </div>
    <div class="reconnect_time rt2" flex="main:center cross:center" v-show="reConnect[1].current">
      {{reConnect[1].current}}
    </div>
    <div class="reconnect_time rt4" flex="main:center cross:center" v-show="reConnect[3].current">
      {{reConnect[3].current}}
    </div>
    <div class="reconnect_time rt3" flex="main:center cross:center" v-show="reConnect[2].current">
      {{reConnect[2].current}}
    </div>


    <!--用户1-->
    <img :src="sites.site1.iconUrl" alt="" class="user_icon ui1" v-show="sites.site1.position"
         @click="showUserInfo(sites.site1,1)">
    <!--用户4-->
    <img :src="sites.site4.iconUrl" alt="" class="user_icon ui4" v-show="sites.site4.position"
         @click="showUserInfo(sites.site4,4)">
    <!--用户3-->
    <img :src="sites.site3.iconUrl" alt="" class="user_icon ui3" v-show="sites.site3.position"
         @click="showUserInfo(sites.site3,3)">
    <!--用户2-->
    <img :src="sites.site2.iconUrl" alt="" class="user_icon ui2" v-show="sites.site2.position"
         @click="showUserInfo(sites.site2,2)">

    <div class="pay_tip" v-if="pay_tip.show">
      <div class="text">
        本游戏工具为收费软件，按游戏时间扣除钻石，一经确认不予退还，是否花费{{this.pay_tip.momeny}}钻石开始牌局？
      </div>
      <a href="javascript:;" @click="do_tip_cancel">取消</a>
      <a href="javascript:;" class="sure" @click="do_sure">确定</a>
    </div>


  </div>
</template>

<script>
  import {mapGetters} from 'vuex'
  import brg from '../../../utils/middle'

  export default {
    name: 'userInfo',
    data() {
      return {
        pay_tip: {
          show: false,
          seat: 0,
          isFirst: true,
          momeny: 0,
        },
        isR: {
          p1: false,
          p2: false,
          p3: false,
          p4: false,
        },
        reConnect: [
          {
            all: 0,
            current: 0,
            timer: null
          }, {
            all: 0,
            current: 0,
            timer: null

          }, {
            all: 0,
            current: 0,
            timer: null
          }, {
            all: 0,
            current: 0,
            timer: null
          }, {
            all: 0,
            current: 0,
            timer: null
          }

        ],
        sites: {site1: {}, site2: {}, site3: {}, site4: {}},
      }
    },
    created() {
      this.userId = this.user_info.id
    },
    mounted() {
      this.dealTimeToMoney()

      brg.$on('ak486',(obj)=>{
        let temp = 0
        let aa = obj
        delete aa.user5Data
        for (let item in aa) {
          this.reConnect[temp].all = aa[item].leftTime
          this.reConnect[temp].current = aa[item].leftTime
          if (aa[item].leftTime > 0) {
            this.doDao(temp)
          } else {
            this.reConnect[temp].timer && clearInterval(this.reConnect[temp].timer)
          }
          if (temp < 4) {
            temp++
          }

        }
      })


      brg.$on('protoMsg', (obj) => {
        let {id, msg} = obj;
        if (id == 209) {
          this.renderNew()
        }
        if (id == 208) {
          if (!msg.result) {
            alert(msg.reason)
            this.pay_tip.isFirst = true
          } else {
            this.renderNew();
          }
        }

      })
      this.renderNew();
    },


    computed: {
      ...mapGetters({
        'user_info': 'user_info',
        'members': 'members',
        'room_id': 'room_id',
        'all_users': 'all_users',
        'bankerSite': 'bankerSite',
        'record': 'record',
        'room_first': 'room_first',
      }),

      showFlower() {
        let obj = [
          {
            isZ:false,
            isY:false
          },
          {
            isZ:false,
            isY:false
          },
          {
            isZ:false,
            isY:false
          },
          {
            isZ:false,
            isY:false
          }
        ]
        let tempUser = Object.assign({}, this.all_users)



        if(typeof(tempUser.user1Data)!== 'undefined'){
          obj = []
        }

        delete tempUser.user5Data
//        console.log(tempUser)



        for (var item in tempUser) {
          let temp = {}
//          console.log(tempUser[item])

          tempUser[item].flower.forEach(v => {
            if (v.type) {
              temp.isZ = v.list.length ? true : false
            } else {
              temp.isY = v.list.length ? true : false
            }
          })
          obj.push(temp)

        }
        console.log(JSON.stringify(obj)+'tempUser')

//        console.log(obj)

        return obj


      },

      //庄家id
      ownerId() {

        let owner = this.members.find(v => v.position == this.bankerSite)
        if (owner) {
          return owner.PlayerId
        }
      },
      //实时更新



    },
    methods: {
      dealTimeToMoney() {

        let time = parseInt(this.room_first.longTime) / 60
        switch (time) {
          case 15:
            this.pay_tip.momeny = 100
            break;
          case 30:
            this.pay_tip.momeny = 150
            break;
          case 60:
            this.pay_tip.momeny = 200
            break;
          default:
            this.pay_tip.momeny = 100
        }

      },
      //显示用户信息
      showUserInfo(obj, site) {
        this.$store.dispatch('showUserModal', {show: true, data: obj})
      },
      //倒计时
      doDao(d) {
        this.reConnect[d].timer && clearInterval(this.reConnect[d].timer)
        this.reConnect[d].current = this.reConnect[d].all
        this.reConnect[d].timer = setInterval(() => {
          this.$nextTick(() => {
            this.reConnect[d].current--;
            if (this.reConnect[d].current <= 0) {
              clearInterval(this.reConnect[d].timer)
            }
          })
        }, 1000)

      },

      renderNew() {
        this.members.find(v => {
          if (v.PlayerId == this.userId) {
            this.changeSeat(v.position)
          }
        })
      },
      //初始化位置
      initSeat() {
        this.members.forEach((v) => {
          if (v.position == 1) {
            this.sites.site1 = v
          } else if (v.position == 2) {
            this.sites.site2 = v
          } else if (v.position == 3) {
            this.sites.site3 = v
          } else if (v.position == 4) {
            this.sites.site4 = v
          }
        })
      },
      //坐下座位
      doSeat(index) {

        let id = this.user_info.id;

        let  ower = this.members.find(v=>v.PlayerId==id)
//        console.log(ower)


        if(ower.isPay){
          this.doActionSeat(index);
        }else{
          this.pay_tip.show = true
          this.pay_tip.seat = index
        }


//        if (this.pay_tip.isFirst) {
//          this.pay_tip.show = true
//          this.pay_tip.seat = index
//        } else {
//          this.doActionSeat(index);
//        }


      },

      do_tip_cancel() {
        this.pay_tip.show = false
        this.pay_tip.seat = ''
      },
      do_sure() {
        this.pay_tip.show = false
        this.doActionSeat(this.pay_tip.seat);
      },

      doActionSeat(index) {
        let nextSite = index
        this.members.find(v => {
          if (v.PlayerId == this.userId) {
            if (v.position != 0) {
              if (v.position == 1) {
                nextSite = index
              } else if (v.position == 2) {
                if (index == 1) {
                  nextSite = 2
                } else if (index == 2) {
                  nextSite = 3
                } else if (index == 3) {
                  nextSite = 4
                } else if (index == 4) {
                  nextSite = 1
                }
              } else if (v.position == 3) {
                if (index == 1) {
                  nextSite = 3
                } else if (index == 2) {
                  nextSite = 4
                } else if (index == 3) {
                  nextSite = 1
                } else if (index == 4) {
                  nextSite = 2
                }
              } else if (v.position == 4) {
                if (index == 1) {
                  nextSite = 4
                } else if (index == 2) {
                  nextSite = 1
                } else if (index == 3) {
                  nextSite = 2
                } else if (index == 4) {
                  nextSite = 3
                }
              }


            }
            let aa = this.members.find(v => v.position == nextSite)
            if (!aa) {
              this.socket.send(207, {roomId: this.room_id, position: nextSite})
            }
          }
        })
      },

      //位置的随便改变
      changeSeat(myPosition) {
        this.sites = {}
        switch (myPosition) {
          case 1:
            this.sites.site1 = this.members.find(v => v.position == 1) ? this.members.find(v => v.position == 1) : {}
            this.sites.site2 = this.members.find(v => v.position == 2) ? this.members.find(v => v.position == 2) : {}
            this.sites.site3 = this.members.find(v => v.position == 3) ? this.members.find(v => v.position == 3) : {}
            this.sites.site4 = this.members.find(v => v.position == 4) ? this.members.find(v => v.position == 4) : {}
            break;
          case 2:
            this.sites.site1 = this.members.find(v => v.position == 2) ? this.members.find(v => v.position == 2) : {}
            this.sites.site2 = this.members.find(v => v.position == 3) ? this.members.find(v => v.position == 3) : {}
            this.sites.site3 = this.members.find(v => v.position == 4) ? this.members.find(v => v.position == 4) : {}
            this.sites.site4 = this.members.find(v => v.position == 1) ? this.members.find(v => v.position == 1) : {}
            break;
          case 3:
            this.sites.site1 = this.members.find(v => v.position == 3) ? this.members.find(v => v.position == 3) : {}
            this.sites.site2 = this.members.find(v => v.position == 4) ? this.members.find(v => v.position == 4) : {}
            this.sites.site3 = this.members.find(v => v.position == 1) ? this.members.find(v => v.position == 1) : {}
            this.sites.site4 = this.members.find(v => v.position == 2) ? this.members.find(v => v.position == 2) : {}
            break;
          case 4:
            this.sites.site1 = this.members.find(v => v.position == 4) ? this.members.find(v => v.position == 4) : {}
            this.sites.site2 = this.members.find(v => v.position == 1) ? this.members.find(v => v.position == 1) : {}
            this.sites.site3 = this.members.find(v => v.position == 2) ? this.members.find(v => v.position == 2) : {}
            this.sites.site4 = this.members.find(v => v.position == 3) ? this.members.find(v => v.position == 3) : {}
            break;
          default:
            this.sites.site1 = this.members.find(v => v.position == 1) ? this.members.find(v => v.position == 1) : {}
            this.sites.site2 = this.members.find(v => v.position == 2) ? this.members.find(v => v.position == 2) : {}
            this.sites.site3 = this.members.find(v => v.position == 3) ? this.members.find(v => v.position == 3) : {}
            this.sites.site4 = this.members.find(v => v.position == 4) ? this.members.find(v => v.position == 4) : {}
        }
      },


    },
    filters: {
      change(val, all) {

        let user = all.find(v => val == v.playerId)
        if (user) {
          if (user.flag) {
            return user.score
          } else {
            return '-' + user.score
          }
        } else {
          return 0
        }
      },
      zhenghua(id, site, all_users) {
        if (typeof(all_users.user1Data) !== 'undefined') {
////          console.log(site)
          let flower = all_users['user' + site + 'Data']['flower'].find(v => v.type == true)
          if (flower) {
            return flower.list.length
          } else {
            return 0
          }
        }

      },
      doReCon(id, site, all_users) {
        if (typeof(all_users.user1Data) !== 'undefined') {
          let leftTime = all_users['user' + site + 'Data']['leftTime']

          let timer = null

          if (leftTime) {
            return leftTime
          } else {
            return 0
          }
        }
      },
      yehua(id, site, all_users) {
        if (typeof(all_users.user1Data) !== 'undefined') {
          let flower = all_users['user' + site + 'Data']['flower'].find(v => v.type == false)
          if (flower) {
            return flower.list.length
          } else {
            return 0
          }

        }

      }
    }


  }

</script>

<style lang="scss" scoped>

  .pay_tip {
    position: fixed;
    top: 50%;
    left: 50%;
    width: 5rem;
    -webkit-transform: translate(-50%, -50%) rotate(-90deg);
    -moz-transform: translate(-50%, -50%) rotate(-90deg);
    -ms-transform: translate(-50%, -50%) rotate(-90deg);
    -o-transform: translate(-50%, -50%) rotate(-90deg);
    transform: translate(-50%, -50%) rotate(-90deg);
    background: #012118;
    padding: .2rem;
    border-radius: 5px;
    z-index: 10000000000;

    .text {
      text-indent: 2em;
      font-size: .26rem;
      line-height: 1.6;
      text-align: left;
      margin-bottom: .3rem;
    }

    a {
      background: #fff;
      color: #219555;
      border-radius: 5px;

      font-size: .24rem;
      padding: .1rem .1rem;
      text-decoration: none;
      display: inline-block;
      width: 2rem;
      margin: .08rem;
      &.sure {
        background: #219555;
        color: #fff;
      }
    }
  }

  .back {
    display: inline-block;
    float: left;
    padding: .2rem;
    border: 1px solid #fff;
  }

  .user_icon {
    position: fixed;
    width: .7rem;
    height: .7rem;
    margin-bottom: -2rem;
    border-radius: .35rem;
    -webkit-transform: rotate(-90deg);
    -moz-transform: rotate(-90deg);
    -ms-transform: rotate(-90deg);
    -o-transform: rotate(-90deg);
    transform: rotate(-90deg);
    border: 1px solid #23b586;

    &.ui1 {
      top: 90.5%;
      left: 72%;
    }
    &.ui2 {
      top: 3%;
      left: 22%;
    }
    &.ui3 {
      top: 79%;
      left: 10%;
    }
    &.ui4 {
      top: 92%;
      left: 40%;
    }
  }

  .user {

    .item {
      z-index: 0;
      position: fixed;
      width: .8rem;
      height: 1.4rem;
      -webkit-transform: rotate(-90deg);
      -moz-transform: rotate(-90deg);
      -ms-transform: rotate(-90deg);
      -o-transform: rotate(-90deg);
      transform: rotate(-90deg);
      font-size: .24rem;
      margin-right: -.2rem;
      overflow: visible;
      .banker {
        position: absolute;
        top: 10%;
        left: 65%;
        width: .6rem;
        font-size: .28rem;
        img {
          width: .43rem;
          height: .54rem;
          border: none;
          margin: 0;
        }

      }
      .point {
        background: rgba(0, 0, 0, .6);
        display: inline-block;
        border-radius: 5px;
        font-weight: 200;
        width: .4rem;
        overflow: visible;
        font-size: .16rem;

      }

      .flower {
        position: fixed;
        top: 1.24rem;
        width: .8rem;
        pointer-events: none;
        strong {
          margin: 0;

        }
        text-align: center;
        font-weight: 200;
        font-size: .16rem;
        img {
          width: .3rem;
          height: .3rem;
          border: none;
          margin-bottom: -.05rem;
        }
        color: #fbda47;
        text-shadow: 0 0 1px rgba(0, 0, 0, 1);
      }

      &.u1 {
        top: 89%;
        left: 40%;
      }
      &.u2 {
        top: 0%;
        left: 22%;
      }
      &.u3 {
        top: 87.5%;
        left: 72%;
      }
      &.u4 {
        top: 76%;
        left: 10%;
      }
      p {
        border: 1px solid #23b586;
        font-size: .2rem;
        width: .8rem;
        height: .8rem;
        border-radius: .4rem;
        color: #23b586;
        font-weight: bold;
        text-shadow: 0 0 5px #000;
        background: #01271d;

      }
      span {
        margin-bottom: .8rem;
        font-size: .2rem;
      }
      img {
        margin-top: .2rem;
        width: .6rem;
        height: .6rem;
        border-radius: .3rem;
        border: 2px solid #e7bb2e;
      }
    }
  }

  .reconnect_time {
    position: fixed;
    width: .8rem;
    height: .8rem;
    z-index: 1000;
    border-radius: 50%;
    color: #f79c03;
    font-size: .4rem;
    font-weight: 400;
    background: rgba(0, 0, 0, .5);
    border: 1px solid #f79c03;
    -webkit-transform: rotate(-90deg);
    -moz-transform: rotate(-90deg);
    -ms-transform: rotate(-90deg);
    -o-transform: rotate(-90deg);
    transform: rotate(-90deg);
    pointer-events: none;

    &.rt4 {
      top: 91%;
      left: 40%;
    }
    &.rt2 {
      top: 2%;
      left: 22%;
    }
    &.rt1 {
      top: 89.5%;
      left: 72%;
    }
    &.rt3 {
      top: 78%;
      left: 10%;
    }
  }
</style>
