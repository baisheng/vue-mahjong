<template>

  <div v-if="showCount.show">
    <div class="count  ">
      <div class="bd">
        <div v-show="showCount.data.isWin">
          <div class="pai">
            <div class="item_g" v-for="group in pailist">
              <div class="item" v-for="item in group">
                <img :src="item" alt="">
              </div>
            </div>
          </div>
          <div class="text" flex="cross:center main:left">
            <div class="item" v-for="n in showCount.data.typeList">
              {{n.type | toStr(type)}}{{n.score}}台
            </div>
          </div>

          <div class="total">
            （合计：{{sumWin}}台）
          </div>
        </div>

        <div v-show="!showCount.data.isWin">
          <img src="../../../assets/new/end.png" alt="" class="endimg">
        </div>

        <div class="user_con" flex="cross:center main:left box:mean">
          <div class="item" v-for="item in userCon">
            <div class="name">
              {{item.name}}
            </div>
            <div class="img_a" v-if="item.img">
              <img :src="item.img" alt="">
            </div>
            <div :class="{'point':10>0,'fail':item.flag==2?true:false }">
              <span v-show="item.flag">{{item.flag == 1 ? '+' : '-' }}</span>{{item.num}}

            </div>
          </div>
        </div>
        <div class="sure">
          <a href="javascript:;" @click="doSure">确定</a>
        </div>
      </div>
    </div>

  </div>

</template>

<script>
  import {mapGetters} from 'vuex'
  import Game from '../game'

  const paoUri = require('../../../assets/new/aa/pao.png')
  const huUri = require('../../../assets/new/aa/hu.png')
  const zimoUri = require('../../../assets/new/aa/zimo.png')

  export default {
    name: 'count',
    data() {
      return {

        card: [],
        type: [
          '没胡',
          '天胡',
          '地胡',
          '八花',
          '清一色',
          '字一色',
          '四花',
          '大吊',
          '对对胡',
          '混一色',
          '正风',
          '正花',
          '平胡',
          '边到',
          '对到',
          '嵌套',
          '单吊',
          '自摸',
          '杠',
          '中发白',
          '圈风',
          '门清',
          '野花',
          '海底捞月'
        ]
      }
    },
    mounted() {


    },
    filters: {
      toStr(val, type) {
        console.log(val)
        return type[val]
      }
    },
    methods: {
      doSure() {
        this.$store.dispatch('showCount', {show: false, data: this.showCount.data})
        this.game = Game.getInstance()
        this.game.getInit()
        this.socket.send(311)
        let date = (new Date()).valueOf();
        if (this.room_status.endTime <= date / 1000) {
          this.$store.dispatch('fetchAllUser', {})
          this.$store.dispatch('fetchOver', {show: true})

        }

      }
    },
    computed: {
      ...mapGetters({
        'showCount': 'showCount',
        'members': 'members',
        'room_status': 'room_status2',
        'room_record_id': 'room_record_id',
      }),
      pailist() {
        let list = []
        if (this.showCount.data.combinationList.length > 1) {
          this.showCount.data.combinationList.forEach((v) => {
            let temp = []
            v.info.forEach((k) => {
              let img = require('../../../assets/card4/' + k + '.png')
              temp.push(img)
            })
            list.push(temp)
          })
        }
        return list
      },
      sumWin() {
        let sum = 0

        if (this.showCount.data.typeList.length >= 1) {
          this.showCount.data.typeList.forEach(v => {
            sum += v.score
          })

        }
        return sum


      },
      userCon() {
        let obj = []
        if (this.showCount.data.list.length > 0) {
          this.showCount.data.list.forEach(v => {
            let temp = v
            let user = this.members.find(k => k.PlayerId === v.id)
            temp.name = user.playerName
            temp.num = v.reward[0].num

            switch (v.type) {
              case 0:
                temp.img = '';
                break;
              case 1:
                temp.img = paoUri;
                break;
              case 2:
                temp.img = huUri;
                break;
              case 3:
                temp.img = zimoUri;
                break;
              default:
                temp.img = '';
            }
            obj.push(temp)
          })
        }

        return obj
      }
    }
  }

</script>

<style scoped lang="scss">
  .animated {
    animation-duration: .3s;
  }
  .img_a{
    position: absolute;
    right: 0;
    bottom: 3px;
    img{
      width: 1rem;
      opacity: .97;
    }
  }
  .count {
    position: fixed;
    top: 50%;
    left: 50%;
    transform-origin: center center;
    -webkit-transform: translate(-50%, -50%) rotate(-90deg);
    -moz-transform: translate(-50%, -50%) rotate(-90deg);
    -ms-transform: translate(-50%, -50%) rotate(-90deg);
    -o-transform: translate(-50%, -50%) rotate(-90deg);
    transform: translate(-50%, -50%) rotate(-90deg);
    width: 9.2rem;
    height: 5.75rem;
    background: rgba(0, 0, 0, .6);
    z-index: 100000000000;
    padding: .1rem;
    border-radius: 5px;
    .bd {
      border-radius: 5px;
      border: 2px solid #b6b023;
      width: 100%;
      height: 100%;
    }

    .user_con {
      .item {
        position: relative;
        .point {
          background-image: -webkit-gradient(linear, 0 0, 0 bottom, from(rgba(249, 251, 0, 1)), to(rgba(255, 51, 0, 1)));
          -webkit-background-clip: text;
          -webkit-text-fill-color: transparent;

          &.fail {
            background-image: -webkit-gradient(linear, 0 0, 0 bottom, from(rgba(23, 254, 251, 1)), to(rgba(30, 158, 240, 1)));
            -webkit-background-clip: text;
            -webkit-text-fill-color: transparent;
          }
        }
      }
    }
    .sure {
      margin-top: .2rem;
      a {
        text-decoration: none;
        display: inline-block;
        padding: .1rem .3rem;
        background: #e49531;
        color: #fff;
        border-radius: 5px;
        letter-spacing: 4px;
        font-size: .28rem;
        box-shadow: 0 0 8px #e49531;
      }
    }
    .user_con {
      .item {
        height: 1.2rem;
        padding: .1rem;
        .name {
          font-size: .2rem;
        }
        .point {
          margin-top: .1rem;
        }
        border: 1px solid #fff100;
        box-shadow: 0 0 7px #fff100;
        border-radius: 5px;
        margin: .2rem;
        text-align: left;
      }
    }

    .pai {
      padding: .1rem;
      width: 120%;
      &:after {
        content: '';
        display: block;
        clear: both;
      }
      .item_g {
        float: left;
        margin-right: .1rem;
        .item {
          float: left;
          img {
            width: .52rem;
          }
        }

      }
    }
    .endimg {
      margin: .5rem auto;
      width: 4rem;
    }
    .text {
      width: 100%;
      padding: .2rem;
      .item {
        margin-right: .3rem;
      }

    }
    .total {
      text-align: right;
      padding: .3rem;
      color: #f1e14b;

    }

  }


</style>
