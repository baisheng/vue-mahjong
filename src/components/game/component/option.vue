<template>

  <div class="option" v-show="cardType.show" flex="main:center cross:center">
    <div class="item" v-for="item in list" @click='doa(item)'>
      <img :src="item.img" class="type"/>
      <div class="chose" flex="main:center cross:center box:mean">
        <img v-for="img in item.infolist" :src="img" alt="">

      </div>

    </div>
    <div class="item" @click="doCancle">
      <img :src='pic_guo' alt="" class="type">
    </div>

  </div>

</template>

<script>
  import {mapGetters} from 'vuex'
  import brg from '../../../utils/middle'

  let pic_111 = require('../../../assets/new/op/111.png')
  let pic_123 = require('../../../assets/new/op/123.png')
  let pic_1111 = require('../../../assets/new/op/1111.png')
  let pic_2222 = require('../../../assets/new/op/1111.png')
  let pic_000 = require('../../../assets/new/op/hu.png')
  let pic_guo = require('../../../assets/new/op/pass.png')

  export default {
    data() {
      return {
        pic_111: pic_111,
        pic_123: pic_123,
        pic_1111: pic_1111,
        pic_2222: pic_2222,
        pic_000: pic_000,
        pic_guo: pic_guo,
      }
    },
    mounted() {
      brg.$on('protoMsg', (obj) => {
        let {msg, id} = obj;

        if (id == 321) {
          if (msg.result) {
            this.$store.dispatch('fetchCardType', {show: false})
          }
        }

      })

    },
    methods: {
      /**
       * 吃碰刚
       * @param data
       */
      doa(data) {

        //判断是否在里面
        let aa = this.cardType.data.find(v => v.type == data.type)


        if (data) {
          delete data.img;
          delete data.infolist;
        }


        if (aa) {
          this.socket.send(304, {meld: data})
        } else {
          this.socket.send(305)
        }
      },
      /**
       * 取消
       */
      doCancle() {
        this.$store.dispatch('fetchCardType', {show: false})
        this.socket.send(309)
      }
    },
    computed: {
      ...mapGetters({
        'cardType': 'cardType'
      }),
      list() {
        let temp = []
        if (this.cardType.ishu) {
          temp.push({
            type: this.pic_000,
            img: this.pic_000
          });
          this.cardType.data && this.cardType.data.forEach((v) => {
            let temp_img = []
            v.info.forEach((k) => {
              let img = require(`../../../assets/card4/${k}.png`)
              temp_img.push(img)
            })
            v.infolist = temp_img
            if (v.type == '111') {
              v.img = this.pic_111
            } else if (v.type == '123') {
              v.img = this.pic_123
            } else if (v.type == '1111') {
              v.img = this.pic_1111
            } else if (v.type == '2222') {
              v.img = this.pic_2222
            } else {
              v.img = this.pic_111
            }
            temp.push(v)
          })
          return temp
        } else {
          this.cardType.data && this.cardType.data.forEach((v) => {
            let temp_img = []
            v.info.forEach((k) => {
              let img = require(`../../../assets/card4/${k}.png`)
              temp_img.push(img)
            })
            v.infolist = temp_img
            if (v.type == '111') {
              v.img = this.pic_111
            } else if (v.type == '123') {
              v.img = this.pic_123
            } else if (v.type == '1111') {
              v.img = this.pic_1111
            } else if (v.type == '2222') {
              v.img = this.pic_2222
            } else {
              v.img = this.pic_111
            }
            temp.push(v)
          })

          return temp

        }


      }
    }
  }

</script>

<style scoped lang="scss">

  .option {
    position: fixed;
    top: 0;
    right: 15%;
    -webkit-transform-origin: 100% 100%;
    -moz-transform-origin: 100% 100%;
    -ms-transform-origin: 100% 100%;
    -o-transform-origin: 100% 100%;
    transform-origin: 100% 100%;
    -webkit-transform: rotate(-90deg);
    -moz-transform: rotate(-90deg);
    -ms-transform: rotate(-90deg);
    -o-transform: rotate(-90deg);
    transform: rotate(-90deg);
    z-index: 100000;

    img{
      border: none;
      outline: none;
    }

    .item {
      height: 2.3rem;
      .chose {
        width: 2.2rem;
        &:after {
          content: '';
          display: block;
          clear: both;
        }
        padding: .2rem;
        img {
          width: .5rem;

        }
      }
      .type {
        width: 1rem;
        height: .78rem;
      }
    }
  }

</style>
