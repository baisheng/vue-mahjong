<template>
  <div>


    <div class="content">

      <div class="part1">
        <div class="title">
          我开的局
        </div>
        <div class="con">

          <div v-if="!my_record.my.length" class="one" flex="main:center cross:center dir:top">
            <p>成为牌局的发起者,邀请好友一起玩！</p>
            <a href="javascript:;" @click="openRoom">我要开房</a>
          </div>
          <div v-if="my_record.my.length" class="two"  >
            <div class="item" v-for=" n in my_record.my" flex="main:center cross:center box:justify">
              <div class="img" flex="main:center cross:center">
                <img :src="user_info.iconUrl" alt="">
              </div>
              <div class="left">
                <p>象山麻将</p>
                <p>(房间 {{n.roomId}})</p>
                <p>{{n.startTime|formatMD}} {{n.totalTime|formatS}}分钟局</p>
              </div>
              <div class="right" flex="dir:top main:center cross:center">
                <p>{{n.endTime|formatHS}}结束</p>
                <a href="javascript:;" @click="enter(n.roomId)">进入</a>
              </div>
            </div>
          </div>


        </div>

      </div>

      <div class="part2">
        <div class="title">
          最近参加的局
        </div>
        <div class="con">
          <div v-if="my_record.other.length" class="two"  >
            <div class="item" v-for=" n in my_record.other" flex="main:center cross:center box:justify">
              <div class="img" flex="main:center cross:center">
                <img :src="user_info.iconUrl" alt="">
              </div>
              <div class="left">
                <p>象山麻将</p>
                <p>(房间 {{n.roomId}})</p>
                <p>{{n.startTime|formatMD}} {{n.totalTime|formatS}}分钟局</p>
              </div>
              <div class="right" flex="dir:top main:center cross:center">
                <p>{{n.endTime|formatHS}}结束</p>
                <a href="javascript:;" @click="enter(n.roomId)">进入</a>
              </div>
            </div>
          </div>

          <p  class="no_tip" v-if="!my_record.other.length">当前没有进行中的牌局</p>
        </div>

      </div>


    </div>

  </div>
</template>

<script>
  import {mapGetters} from 'vuex'
  import brg from '../../utils/middle'
  const moment = require('moment')

  export default {
    name: 'discover',
    data() {
      return {
        isIn: false,
        isOpen: false
      }
    },
    computed: {
      ...mapGetters({
        'room_id': 'room_id',
        'my_record': 'my_record',
        'user_info': 'user_info',
      })
    },

    beforeMount(){
      this.socket.send(505)

      brg.$on('protoMsg', (obj) => {
        let {id, msg} = obj
        if(id==506){
          this.$store.dispatch('fetchMyRecord',msg)
        }
      })
    },
    mounted(){

    },
    filters: {
      formatMD(val) {
        return moment(val * 1000).format('MM-DD')
      },
      formatMDHS(val) {
        return moment(val * 1000).format('MM-DD HH:mm')
      },
      formatHS(val) {
        return moment(val * 1000).format('HH:mm')
      },
      formatS(val) {
        return val / 60
      }
    },
    methods: {
      go() {
        this.$router.push({path: '/room'})
      },
      enter(id){
        console.log(id)
        this.socket.send(201,{
          roomId:id,
          type:123,
          longTime:219
        })
      },

      openRoom() {
        this.$router.push({path:'/room2'})
      }
    }
  }
</script>

<style scoped lang="scss">
  .content {
    border: 1px solid #000;
    margin: .48rem .45rem;
    padding: .3rem .4rem;
    text-align: left;
    background: #ffe8bc;
    border: 2px solid #500047;
    border-radius: 8px;

    .title {
      margin-top: .1rem;
      color: #6a3906;
      font-size: .26rem;
      font-weight: 600;
      margin-bottom: .16rem;
      padding-left: .12rem;
    }

    .part1 {
      .con {
        box-shadow: 0 0 15px rgba(0, 0, 0, 1) inset;
        border: 2px solid #cfa972;
        border-radius: 10px;
        background: #6a3906;
        padding: .3rem .2rem;
        height: 3.3rem;
        overflow: auto;
        .one {
          width: 100%;
          height: 100%;

          p {
            font-size: .28rem;
          }

          a {
            display: inline-block;
            background: #e60111;
            color: #fff;
            text-decoration: none;
            margin-top: .35rem;
            padding: .12rem .26rem;
            font-size: .25rem;
            box-shadow: 0 0 15px rgba(0, 0, 0, .3);
            border-radius: 5px;
            font-weight: 600;
            letter-spacing: 2px;
          }

        }

        .two {
          width: 100%;
          height: 100%;
          .item{
            width: 100%;
            margin-bottom: .3rem;
          }
          .img {
            width: 1.6rem;
            height: 100%;
            text-align: center;
            img {
              width: 1.2rem;
              height: 1.2rem;
              border-radius: 8px;
              border: 2px solid #fdd20f;

            }
          }
          .left {
            color: #d2aa78;
            font-size: .24rem;
            line-height: 1.5;
          }
          .right {
            color: #d2aa78;
            font-size: .24rem;
            margin-right: .1rem;
            a {
              text-decoration: none;
              color: #fff;
              display: inline-block;
              padding: .06rem .2rem;
              box-shadw: 0 0 15px rgba(0, 0, 0, .3);
              background: #e60012;
              border-radius: 4px;
              margin-top: .1rem;
            }
          }
        }
      }
    }

    .part2 {
      margin-top: .2rem;
      .con {
        box-shadow: 0 0 15px rgba(0, 0, 0, 1) inset;
        border: 2px solid #cfa972;
        border-radius: 10px;
        background: #6a3906;
        padding: .3rem .2rem;
        overflow: auto;
        min-height: 2rem;
        position: relative;

        .two {
          width: 100%;
          height: 100%;
          .item{
            width: 100%;
            margin-bottom: .3rem;
          }
          .img {
            width: 1.6rem;
            height: 100%;
            text-align: center;
            img {
              width: 1.2rem;
              height: 1.2rem;
              border-radius: 8px;
              border: 2px solid #fdd20f;

            }
          }
          .left {
            color: #d2aa78;
            font-size: .24rem;
            line-height: 1.5;
          }
          .right {
            color: #d2aa78;
            font-size: .24rem;
            margin-right: .1rem;
            a {
              text-decoration: none;
              color: #fff;
              display: inline-block;
              padding: .06rem .2rem;
              box-shadw: 0 0 15px rgba(0, 0, 0, .3);
              background: #e60012;
              border-radius: 4px;
              margin-top: .1rem;
            }
          }
        }

        .no_tip {
          position: absolute;
          top: 50%;
          left: 50%;
          -webkit-transform: translate(-50%, -50%);
          -moz-transform: translate(-50%, -50%);
          -ms-transform: translate(-50%, -50%);
          -o-transform: translate(-50%, -50%);
          transform: translate(-50%, -50%);
          color: #756554;
          font-size: .26rem;
        }
      }

    }
  }
</style>
